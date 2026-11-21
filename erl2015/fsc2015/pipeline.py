"""End-to-end run: networks, metrics, disturbance, and tables on disk."""

from __future__ import annotations

from dataclasses import dataclass, field
from pathlib import Path

import numpy as np
import pandas as pd

from . import config, data, metrics
from .disturbance import ban_vector, disturb
from .network import build_networks

MT = metrics.MT


@dataclass
class Inputs:
    trade: pd.DataFrame
    countries: pd.DataFrame
    gdp: pd.DataFrame
    supply: pd.DataFrame
    ldc: pd.DataFrame
    exclusions: pd.DataFrame

    @classmethod
    def load(cls) -> "Inputs":
        trade = data.load_trade()
        return cls(trade=trade, countries=data.country_table(trade), gdp=data.load_gdp(),
                   supply=metrics.staple_supply(data.load_food_balances()), ldc=data.load_ldc(),
                   exclusions=data.load_ldc_exclusions())

    def gdp_for(self, year: int) -> pd.Series:
        g = self.gdp[self.gdp["year"] == year]
        return pd.Series(g["value"].values, index=g["fao"].values)

    def supply_for(self, year: int) -> pd.Series:
        s = self.supply[self.supply["year"] == year]
        return pd.Series(s["supply_t"].values, index=s["fao"].values)


@dataclass
class RunResult:
    crop: str
    scenario: str
    periods: dict
    tables: dict = field(default_factory=dict)


def _years(periods: dict) -> list[int]:
    return sorted({y for a, b in periods.values() for y in range(a, b + 1)})


def check_coverage(inp: Inputs, years: list[int]) -> list[str]:
    """Warnings about years that lack GDP or food balance data."""
    notes = []
    for name, df in (("GDP", inp.gdp), ("food balances", inp.supply)):
        have = set(df["year"].unique())
        gap = [y for y in years if y not in have]
        if gap:
            notes.append(f"{name} missing for {gap}")
    tr = set(inp.trade["year"].unique())
    gap = [y for y in years if y not in tr]
    if gap:
        raise data.DataError(f"Trade missing for {gap}")
    return notes


def run(crop: str, scenario: str, periods: dict | None = None, inp: Inputs | None = None,
        mirror: str = "mean", min_flow_t: float = 1.0) -> RunResult:
    periods = periods or config.DEFAULT_PERIODS
    inp = inp or Inputs.load()
    years = _years(periods)
    notes = check_coverage(inp, years)
    scen = data.load_scenario(scenario)
    nets = build_networks(inp.trade, crop, years, inp.countries, mirror=mirror)

    unmatched = sorted(set(scen["m49"]) - {str(v).zfill(3) for v in inp.countries["m49"]})
    if unmatched:
        notes.append(f"scenario M49 codes with no trade in any year: {unmatched}")

    rows_before, rows_after, country_rows, bans = [], [], [], []
    for y in years:
        net = nets[y]
        f = ban_vector(net, scen)
        res = disturb(net, f, inp.gdp_for(y), inp.supply_for(y))
        rows_before.append(metrics.network_summary(res.before))
        rows_after.append(metrics.network_summary(res.after))
        country_rows.append(res.countries)
        bans.append({"year": y, "banned_exporters": int(((f > 0) & (net.exports > 0)).sum()),
                     "lost_trade_mt": res.lost_trade_t / MT})

    before, after = pd.DataFrame(rows_before), pd.DataFrame(rows_after)
    countries = pd.concat(country_rows, ignore_index=True)
    out = RunResult(crop, scenario, periods)
    out.tables["yearly_before"] = before
    out.tables["yearly_after"] = after
    out.tables["bans"] = pd.DataFrame(bans)
    out.tables["countries_yearly"] = countries
    out.tables["notes"] = pd.DataFrame({"note": notes})

    # Table 1 and Table 2 analogues.
    t1, t2 = {}, {}
    for label, (a, b) in periods.items():
        mb, ma = metrics.period_mean(before, a, b), metrics.period_mean(after, a, b)
        t1[label] = mb
        t2[f"{label} loss"] = mb - ma
        t2[f"{label} loss %"] = np.where(mb != 0, 100 * (mb - ma) / mb.replace(0, np.nan), np.nan)
    out.tables["table1_network"] = pd.DataFrame(t1).rename_axis("metric").reset_index()
    out.tables["table2_disturbance"] = pd.DataFrame(t2).rename_axis("metric").reset_index()

    # Country losses averaged over each period, then LDC summaries.
    ldc = set(inp.ldc["m49"])
    per, ldc_rows = [], []
    for label, (a, b) in periods.items():
        sel = countries[(countries["year"] >= a) & (countries["year"] <= b)]
        g = sel.groupby(["fao", "name", "m49"], as_index=False).agg(
            imports_t=("imports_t", "mean"), static_loss_t=("static_loss_t", "mean"),
            dynamic_loss_t=("dynamic_loss_t", "mean"), staple_supply_t=("staple_supply_t", "mean"),
            static_loss_share=("static_loss_share", "mean"),
            dynamic_loss_share=("dynamic_loss_share", "mean"))
        g["period"] = label
        g["ldc"] = g["m49"].astype(str).str.zfill(3).isin(ldc)
        # A country whose imports of this one crop exceed its whole staple
        # supply is passing grain on (a transit hub, e.g. Djibouti); its loss
        # share is not a consumption loss. Flag it, and the countries listed in
        # ancillary/ldc_summary_exclusions.csv, and leave them out of summaries.
        g["transit_flag"] = (g["imports_t"] > g["staple_supply_t"]) | \
            g["m49"].astype(str).str.zfill(3).isin(set(inp.exclusions["m49"]))
        per.append(g)
        L = g[g["ldc"] & g["imports_t"].gt(0) & g["static_loss_share"].notna() & ~g["transit_flag"]]
        ldc_rows.append({
            "period": label, "n_ldc": len(L),
            "excluded_transit": ", ".join(g.loc[g["ldc"] & g["transit_flag"], "name"]),
            "static_median_pct": 100 * L["static_loss_share"].median(),
            "static_mean_pct": 100 * L["static_loss_share"].mean(),
            "dynamic_median_pct": 100 * L["dynamic_loss_share"].median(),
            "dynamic_mean_pct": 100 * L["dynamic_loss_share"].mean(),
        })
    out.tables["countries_by_period"] = pd.concat(per, ignore_index=True)
    out.tables["ldc_summary"] = pd.DataFrame(ldc_rows)

    # Top exporters, betweenness, power law in the last year of each period.
    top, btw, plaw, pnodes = [], [], [], []
    for label, (a, b) in periods.items():
        nt = metrics.node_table(nets[b]).sort_values("s_out_t", ascending=False).head(10)
        nt["period"] = label
        top.append(nt)
        bt = metrics.betweenness(nets[b]).head(10)
        bt["period"] = label
        btw.append(bt)
        pn = metrics.period_nodes(nets, a, b)
        fit = metrics.power_law_fit(pn)
        fit["period"] = label
        plaw.append(fit)
        pnodes.append(pn.assign(period=label))
    out.tables["top_exporters"] = pd.concat(top, ignore_index=True)
    out.tables["top_betweenness"] = pd.concat(btw, ignore_index=True)
    out.tables["power_law"] = pd.DataFrame(plaw)
    out.tables["period_nodes"] = pd.concat(pnodes, ignore_index=True)
    return out


def timeseries(inp: Inputs, crop: str, start: int, end: int, mirror: str = "mean",
               min_flow_t: float = 1.0) -> pd.DataFrame:
    """Yearly network summary over a span of years (figure S2 analogue)."""
    have = sorted(set(inp.trade["year"]))
    years = [y for y in range(start, end + 1) if y in have]
    nets = build_networks(inp.trade, crop, years, inp.countries, mirror=mirror, min_flow_t=min_flow_t)
    df = metrics.summarize_years(nets)
    df["crop"] = crop
    return df


def ssr_by_period(inp: Inputs, periods: dict | None = None) -> pd.DataFrame:
    periods = periods or config.DEFAULT_PERIODS
    rows = []
    for label, (a, b) in periods.items():
        s = inp.supply[(inp.supply["year"] >= a) & (inp.supply["year"] <= b)]
        g = s.groupby("fao", as_index=False)[["production_t", "supply_t"]].mean()
        g = g[g["supply_t"] > 0]
        g["ssr"] = g["production_t"] / g["supply_t"]
        g["period"] = label
        rows.append(g)
    out = pd.concat(rows, ignore_index=True)
    names = dict(zip(inp.countries["fao"], inp.countries["name"]))
    out["name"] = out["fao"].map(names)
    out["class"] = metrics.ssr_classes(out["ssr"]).astype(str)
    return out


def write(result: RunResult, root: Path = config.OUTPUT_DIR) -> Path:
    d = root / f"{result.crop}_{result.scenario}"
    d.mkdir(parents=True, exist_ok=True)
    for name, tab in result.tables.items():
        if name == "countries_yearly":
            continue  # large; the period table carries what the report needs
        tab.to_csv(d / f"{name}.csv", index=False, float_format="%.6g")
    return d

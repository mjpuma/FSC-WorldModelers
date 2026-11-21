#!/usr/bin/env python3
"""Figures and the flood-shock report.

Reads outputs/flood_shocks and the annual network files. Writes the figures
and REPORT.md under outputs/flood_shocks, plus a Word copy of the same text.

Every number in the report text is computed here from the run tables
(flood_shock_summary.csv, flood_shock_countries.csv, flood_shock_edges.csv),
the shock table the run used (shock_table_used.csv), or the annual network
files the run was built from. Qualitative sentences that depend on a number
are guarded by require(), so the script stops instead of printing a sentence
the data no longer supports.
"""

from pathlib import Path

import geopandas as gpd
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from cartopy.io import shapereader
from docx import Document
from docx.shared import Inches, Pt
from matplotlib.colors import PowerNorm
from matplotlib.lines import Line2D

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / "outputs" / "flood_shocks"
FIG = OUT / "figures"
ANNUAL = ROOT / "outputs" / "export_for_RIKEN" / "annual_networks_2002_2024"
CROPS = ["rice_milled", "wheat", "maize"]
CROP_LABEL = {"rice_milled": "Milled rice", "wheat": "Wheat", "maize": "Maize"}
CROP_LOWER = {"rice_milled": "milled rice", "wheat": "wheat", "maize": "maize"}
CROP_COLOR = {"rice_milled": "#9b2226", "wheat": "#bb3e03", "maize": "#005f73"}
SOURCE = "Draft flood-shock table dated 20 November 2026. Annual networks, 2002 to 2024. Metric tonnes."
SPLIT = "stock_then_split"
PTA = "modified_pta"
GFD_LAST = 2014
PARTNER_MIN_USE = 1e5
HOME_FLOOR = 0.6
STU_BINS = [0, 0.05, 0.2, 0.5, np.inf]
STU_LABELS = ["Under 0.05", "0.05 to 0.20", "0.20 to 0.50", "0.50 and above"]
STRESS_FACTORS = [2, 5, 10]
OBS_EVENTS = [
    ("PAK", "rice_milled", 2010),
    ("PAK", "wheat", 2010),
    ("PAK", "rice_milled", 2022),
    ("PAK", "wheat", 2022),
    ("THA", "rice_milled", 2011),
    ("BGD", "rice_milled", 2004),
    ("BGD", "rice_milled", 2007),
]
CHANNELS = [
    ("production_mt", "Production"),
    ("stock_change", "Stock change"),
    ("domestic_use_mt", "Domestic use"),
    ("net_imports", "Net imports"),
]
MIN_NOISE_YEARS = 8
CONTINENTS = ["Asia", "Africa", "Europe", "North America", "South America", "Oceania"]
# Countries in the 195-country network that the 110m Natural Earth map leaves out.
CONTINENT_FIX = {
    "ANT": "North America", "ARB": "North America", "ATG": "North America", "BMU": "North America",
    "BRB": "North America", "DMA": "North America", "GRD": "North America", "KNA": "North America",
    "LCA": "North America", "VCT": "North America", "BHR": "Asia", "HKG": "Asia", "MAC": "Asia",
    "MDV": "Asia", "SGP": "Asia", "COM": "Africa", "CPV": "Africa", "MUS": "Africa", "STP": "Africa",
    "SYC": "Africa", "FRO": "Europe", "MLT": "Europe", "COK": "Oceania", "KIR": "Oceania",
    "PYF": "Oceania", "TON": "Oceania", "TUV": "Oceania",
}
BENCHMARKS = [
    ("THA", "rice_milled", 2011),
    ("MMR", "rice_milled", 2008),
    ("PAK", "wheat", 2022),
    ("PAK", "rice_milled", 2022),
    ("USA", "maize", 2011),
    ("USA", "wheat", 2011),
]


def require(condition, message):
    if not condition:
        raise SystemExit(f"Report claim no longer holds: {message}")


# Formatting -----------------------------------------------------------------

def mt(value, digits=2):
    return f"{value / 1e6:.{digits}f} Mt"


def kt(value, digits=0):
    return f"{value / 1e3:,.{digits}f} kt"


def pct(value, digits=1):
    return f"{100 * value:.{digits}f}%"


def tonnes(value):
    return f"{value:,.0f} t"


def joined(items):
    items = list(items)
    if len(items) <= 1:
        return "".join(items)
    if len(items) == 2:
        return f"{items[0]} and {items[1]}"
    return ", ".join(items[:-1]) + ", and " + items[-1]


def by_crop(values, fmt):
    return joined(f"{CROP_LOWER[c]} {fmt(values[c])}" for c in CROPS)


# Inputs -----------------------------------------------------------------------

def names_table():
    lookup = pd.read_csv(ROOT / "outputs" / "export_for_RIKEN" / "country_lookup_ISO3_FAO.csv")
    return dict(zip(lookup.iso3, lookup.country_name))


def short_name(names, iso):
    name = str(names.get(iso, iso)).split("(")[0].strip()
    return {
        "United States of America": "United States",
        "Russian Federation": "Russia",
        "Cote dIvoire": "Cote d'Ivoire",
        "Viet Nam": "Vietnam",
        "Iran, Islamic Republic of": "Iran",
    }.get(name, name)


def world_frame():
    path = shapereader.natural_earth(resolution="110m", category="cultural", name="admin_0_countries")
    world = gpd.read_file(path)
    iso_col = "ISO_A3" if "ISO_A3" in world.columns else "ADM0_A3"
    name_col = "NAME" if "NAME" in world.columns else "ADMIN"
    world = world.rename(columns={iso_col: "iso3", name_col: "name"})
    fix = {"France": "FRA", "Norway": "NOR", "Somaliland": "SOM", "Kosovo": "XKX"}
    for name, iso in fix.items():
        world.loc[world["name"] == name, "iso3"] = iso
    world = world[world["iso3"].notna() & ~world["iso3"].isin(["-99", ""])].copy()
    points = world.geometry.representative_point()
    world["lon"] = points.x
    world["lat"] = points.y
    return world


def load_inputs():
    summary = pd.read_csv(OUT / "flood_shock_summary.csv")
    countries = pd.read_csv(OUT / "flood_shock_countries.csv")
    edges = pd.read_csv(OUT / "flood_shock_edges.csv")
    shocks = pd.read_csv(OUT / "shock_table_used.csv")
    reserves, production, trade = [], [], []
    for crop in CROPS:
        for store, stem in ((reserves, "reserves"), (production, "production"), (trade, "bilateral_trade")):
            frame = pd.read_csv(ANNUAL / f"{crop}_{stem}_2002_2024_mt.csv")
            frame["crop"] = crop
            store.append(frame)
    return (
        summary, countries, edges, shocks,
        pd.concat(reserves, ignore_index=True),
        pd.concat(production, ignore_index=True),
        pd.concat(trade, ignore_index=True),
    )


def baseline_nodes(production, trade):
    """Exports and network use per crop, year, and country, built the way run_flood_shocks.R builds them."""
    out = []
    for (crop, year), prod in production.groupby(["crop", "year"]):
        iso = sorted(prod.iso3.unique())
        P = prod.set_index("iso3").production_mt.reindex(iso).fillna(0)
        T = trade[(trade.crop == crop) & (trade.year == year)]
        T = T[T.exporter_iso3.isin(iso) & T.importer_iso3.isin(iso) & (T.exporter_iso3 != T.importer_iso3)]
        X = T.groupby("exporter_iso3").trade_mt_net.sum().reindex(iso, fill_value=0)
        M = T.groupby("importer_iso3").trade_mt_net.sum().reindex(iso, fill_value=0)
        frame = pd.DataFrame({"iso3": iso, "exports_mt": X.values, "imports_mt": M.values, "use_mt": (P + M - X).values})
        frame["crop"] = crop
        frame["year"] = year
        out.append(frame)
    return pd.concat(out, ignore_index=True)


def pick(frame, rule, alpha):
    return frame[(frame.allocation_rule == rule) & (frame.alpha == alpha)]


# Facts --------------------------------------------------------------------------

def share(num, den):
    return num / den if den else float("nan")


def continent_map(world, isos):
    base = world.drop_duplicates("iso3").set_index("iso3").CONTINENT.to_dict()
    out = {}
    for iso in isos:
        out[iso] = CONTINENT_FIX.get(iso, base.get(iso, "Unassigned"))
    return out


def magnitude(f, shocks, production, reserves, cont):
    prod = production[["crop", "iso3", "year", "production_mt"]].copy()
    stock = reserves[["crop", "iso3", "year", "beginning_stocks_mt"]].copy()
    base = prod.merge(stock, on=["crop", "iso3", "year"], how="left")
    base["beginning_stocks_mt"] = base.beginning_stocks_mt.fillna(0)
    base["production_mt"] = base.production_mt.fillna(0)
    base["continent"] = base.iso3.map(cont)
    loss = shocks[["crop", "iso3", "year", "shock_total_t"]].copy()
    loss["continent"] = loss.iso3.map(cont)
    f["unassigned"] = sorted(set(base.loc[base.continent == "Unassigned", "iso3"]))
    n_years = base.year.nunique()
    rows = []
    for crop in CROPS:
        b = base[base.crop == crop]
        l = loss[loss.crop == crop]
        for region in CONTINENTS + ["World"]:
            bb = b if region == "World" else b[b.continent == region]
            ll = l if region == "World" else l[l.continent == region]
            P, S, Lt = bb.production_mt.sum(), bb.beginning_stocks_mt.sum(), ll.shock_total_t.sum()
            rows.append({"crop": crop, "region": region, "prod_yr": P / n_years, "stock_yr": S / n_years,
                         "loss_yr": Lt / n_years, "loss_prod": Lt / P if P else np.nan, "loss_stock": Lt / S if S else np.nan})
    f["mag"] = pd.DataFrame(rows)
    f["mag_years"] = n_years
    world = base.groupby(["crop", "year"])[["production_mt", "beginning_stocks_mt"]].sum()
    wl = shocks.groupby(["crop", "year"]).shock_total_t.sum()
    world = world.join(wl).fillna({"shock_total_t": 0})
    world["loss_prod"] = world.shock_total_t / world.production_mt
    world["loss_stock"] = world.shock_total_t / world.beginning_stocks_mt
    f["world_year"] = world
    f["world_prod_all"] = base.production_mt.sum()
    f["world_prod_crop"] = base.groupby("crop").production_mt.sum()
    f["world_stock_crop"] = base.groupby("crop").beginning_stocks_mt.sum()
    f["max_loss_prod"] = world.loss_prod.idxmax(), float(world.loss_prod.max())
    rel = world.production_mt.groupby(level="crop").pct_change()
    f["world_prod_sd"] = rel.groupby(level="crop").std()
    gfd = world.index.get_level_values("year") <= GFD_LAST
    f["period_loss_prod"] = {
        "GFD": world[gfd].groupby("crop").shock_total_t.sum() / world[gfd].groupby("crop").production_mt.sum(),
        "Sentinel": world[~gfd].groupby("crop").shock_total_t.sum() / world[~gfd].groupby("crop").production_mt.sum(),
    }


def observed_check(f, shocks, production, reserves, nodes):
    """Year-to-year changes in observed national series around flood years."""
    obs = nodes.merge(production[["crop", "iso3", "year", "production_mt"]], on=["crop", "iso3", "year"])
    obs = obs.merge(reserves[["crop", "iso3", "year", "beginning_stocks_mt", "domestic_use_mt"]], on=["crop", "iso3", "year"], how="left")
    obs = obs.sort_values(["crop", "iso3", "year"]).reset_index(drop=True)
    obs["net_imports"] = obs.imports_mt - obs.exports_mt
    obs["stock_change"] = obs.groupby(["crop", "iso3"]).beginning_stocks_mt.shift(-1) - obs.beginning_stocks_mt
    for col, _ in CHANNELS:
        obs["d_" + col] = obs.groupby(["crop", "iso3"])[col].diff()
    keyed = {key: frame for key, frame in obs.groupby(["crop", "iso3"])}

    def noise(crop, iso, year, col):
        frame = keyed.get((crop, iso))
        if frame is None:
            return np.nan
        vals = frame.loc[~frame.year.isin([year, year + 1]), "d_" + col].dropna()
        return float(vals.std()) if len(vals) >= MIN_NOISE_YEARS else np.nan

    events = []
    for iso, crop, year in OBS_EVENTS:
        L = shocks[(shocks.iso3 == iso) & (shocks.crop == crop) & (shocks.year == year)].shock_total_t.sum()
        row = obs[(obs.crop == crop) & (obs.iso3 == iso) & (obs.year == year)].iloc[0]
        rec = {"iso3": iso, "crop": crop, "year": year, "loss": L}
        for col, _ in CHANNELS:
            rec["d_" + col] = row["d_" + col]
            rec["sd_" + col] = noise(crop, iso, year, col)
        rec["max_ratio"] = max(L / rec["sd_" + c] for c, _ in CHANNELS if rec["sd_" + c] > 0)
        events.append(rec)
    f["obs_events"] = pd.DataFrame(events)
    f["obs_events_detect"] = int((f["obs_events"].max_ratio >= 2).sum())

    rows = []
    for r in shocks.itertuples(index=False):
        rec = {"loss": r.shock_total_t}
        for col, _ in CHANNELS:
            rec[col] = noise(r.crop, r.iso3, r.year, col)
        rows.append(rec)
    det = pd.DataFrame(rows)
    f["det_n"] = int(det.production_mt.notna().sum())
    f["det_prod_1"] = int((det.loss > det.production_mt).sum())
    f["det_prod_2"] = int((det.loss > 2 * det.production_mt).sum())
    f["det_prod_median"] = float((det.loss / det.production_mt).median())
    sub = det.dropna(subset=[c for c, _ in CHANNELS])
    f["det_any_n"] = len(sub)
    f["det_any_2"] = int((sub.loss.values[:, None] > 2 * sub[[c for c, _ in CHANNELS]].values).any(axis=1).sum())

    # Pooled panel: change in each series on the flood loss, within crop-country.
    loss = shocks.groupby(["crop", "iso3", "year"]).shock_total_t.sum().rename("L").reset_index()
    panel = obs.merge(loss, on=["crop", "iso3", "year"], how="left").fillna({"L": 0})
    flooded = set(zip(shocks.crop, shocks.iso3))
    panel = panel[[(c, i) in flooded for c, i in zip(panel.crop, panel.iso3)]]
    panel = panel[panel.year > panel.year.min()]

    def fe(frame, col):
        d = frame[["crop", "iso3", "L", "d_" + col]].dropna().copy()
        g = d.groupby(["crop", "iso3"])
        d["y"] = d["d_" + col] - g["d_" + col].transform("mean")
        d["x"] = d.L - g.L.transform("mean")
        sxx = (d.x ** 2).sum()
        b = (d.x * d.y).sum() / sxx
        d["u"] = d.y - b * d.x
        G = d.groupby(["crop", "iso3"]).ngroups
        meat = d.assign(s=d.x * d.u).groupby(["crop", "iso3"]).s.sum().pow(2).sum()
        return {"b": b, "se": np.sqrt(meat) / sxx * np.sqrt(G / (G - 1)), "n": len(d), "groups": G}

    pooled = {}
    for label, frame in (("All flooded crop-countries", panel), ("Without China and India", panel[~panel.iso3.isin(["CHN", "IND"])])):
        pooled[label] = {col: fe(frame, col) for col, _ in CHANNELS}
    f["pooled"] = pooled
    f["panel_years"] = (int(panel.year.min()), int(panel.year.max()))

    verdict = {}
    for label, fits in pooled.items():
        out = {}
        for name, pred in list(f["end_members"].items()) + list(f["implied"].items()):
            far = [col for col in pred if abs(pred[col] - fits[col]["b"]) > 2 * fits[col]["se"]]
            out[name] = far
        verdict[label] = out
    f["verdict"] = verdict


def build_facts(summary, countries, edges, shocks, reserves, production, trade, cont):
    f = {}
    s0, s1 = pick(summary, SPLIT, 0), pick(summary, SPLIT, 1)
    p0, p1 = pick(summary, PTA, 0), pick(summary, PTA, 1)
    c0, c1 = pick(countries, SPLIT, 0), pick(countries, SPLIT, 1)
    pc0 = pick(countries, PTA, 0)

    # Shock table.
    f["n_rows"] = len(shocks)
    f["n_iso_years"] = shocks.groupby(["iso3", "year"]).ngroups
    f["n_crop_years"] = shocks.groupby(["crop", "year"]).ngroups
    f["n_years"] = shocks.year.nunique()
    f["year_min"], f["year_max"] = int(shocks.year.min()), int(shocks.year.max())
    f["n_crops"] = shocks.crop.nunique()
    f["n_iso3"] = shocks.iso3.nunique()
    net_iso = set(production.iso3)
    f["net_size"] = production.groupby(["crop", "year"]).iso3.nunique().max()
    f["net_size_min"] = production.groupby(["crop", "year"]).iso3.nunique().min()
    f["all_in_net"] = set(shocks.iso3) <= net_iso
    f["table_total"] = shocks.groupby("crop").shock_total_t.sum()
    capped = shocks[shocks.capped_at_production.astype(str).str.lower().isin(["true", "1"])]
    f["capped"] = capped.sort_values(["year", "iso3", "crop"])
    y0, y1 = shocks.shock_y0_t.sum(), shocks.shock_y1_t.sum()
    f["y0_share"] = y0 / (y0 + y1)
    gfd = shocks.year <= GFD_LAST
    f["gfd_t"] = shocks[gfd].groupby("crop").shock_total_t.sum()
    f["prov_t"] = shocks[~gfd].groupby("crop").shock_total_t.sum()
    f["prov_share"] = f["prov_t"] / (f["gfd_t"] + f["prov_t"])
    f["prov_share_all"] = shocks.loc[~gfd, "shock_total_t"].sum() / shocks.shock_total_t.sum()
    f["first_prov_y1"] = shocks.loc[shocks.year == GFD_LAST + 1, "shock_y1_t"].sum()
    f["top_shock"] = shocks.groupby(["crop", "iso3"]).shock_total_t.sum().reset_index().sort_values("shock_total_t", ascending=False)

    # Runs.
    f["n_runs"] = len(summary)
    f["n_rules"] = summary.allocation_rule.nunique()
    f["alphas"] = sorted(summary.alpha.unique())
    f["split_runs"] = int((summary.allocation_rule == SPLIT).sum())
    f["split_converged"] = int(summary.loc[summary.allocation_rule == SPLIT, "converged"].sum())
    f["split_max_iter"] = int(summary.loc[summary.allocation_rule == SPLIT, "iterations"].max())
    f["pta0_n"] = len(p0)
    f["pta0_cap"] = int((~p0.converged).sum())
    f["pta0_iter_cap"] = int(p0.iterations.max())
    f["pta1_converged"] = int(p1.converged.sum())
    f["pta1_n"] = len(p1)
    f["new_links"] = int(summary.new_links.sum())
    f["overdraw"] = int(summary.reserve_overdraw.sum())
    f["negatives"] = int(summary[["neg_production", "neg_reserves", "neg_trade", "neg_use"]].sum().sum())
    f["split_mass_t"] = summary.loc[summary.allocation_rule == SPLIT, "mass_gap_mt"].abs().max()
    pta_runs = summary[summary.allocation_rule == PTA]
    f["pta_mass_row"] = pta_runs.loc[pta_runs.mass_gap_mt.abs().idxmax()]
    f["pta1_mass_t"] = p1.mass_gap_mt.abs().max()

    # Production loss.
    f["prod"] = s0.groupby("crop").production_loss_mt.sum()
    f["prod_all"] = f["prod"].sum()

    # Result 1. Stocks open.
    g1 = s1.groupby("crop")
    f["stock_share"] = g1.reserve_draw_mt.sum() / g1.production_loss_mt.sum()
    f["remain"] = -g1.net_consumption_change_mt.sum()
    f["remain_all"] = f["remain"].sum()
    f["draw_home"] = g1.direct_reserve_draw_mt.sum() / g1.reserve_draw_mt.sum()
    f["partner_draw_all"] = s1.reserve_draw_mt.sum() - s1.direct_reserve_draw_mt.sum()
    f["remain_home"] = g1.direct_consumption_loss_mt.sum() / g1.world_consumption_loss_mt.sum()
    f["stock_share_all"] = s1.reserve_draw_mt.sum() / s1.production_loss_mt.sum()
    yr = s1.assign(ss=s1.reserve_draw_mt / s1.production_loss_mt, rem=-s1.net_consumption_change_mt)
    f["s1_by_year"] = yr
    f["min_stock_year"] = yr.loc[yr.ss.idxmin()]
    f["max_remain_year"] = yr.loc[yr.rem.idxmax()]
    f["min_remain_home_year"] = yr.loc[yr.direct_loss_share.idxmin()]

    # Result 2. No stocks, split.
    g0 = s0.groupby("crop")
    f["home0"] = g0.direct_consumption_loss_mt.sum() / g0.world_consumption_loss_mt.sum()
    f["world0"] = g0.world_consumption_loss_mt.sum()
    f["net0"] = -g0.net_consumption_change_mt.sum()
    f["low_years"] = s0[s0.direct_loss_share < HOME_FLOOR].sort_values(["crop", "year"])
    f["min_home0"] = g0.direct_loss_share.min()
    wheat = s0[s0.crop == "wheat"].set_index("year")
    f["wheat_year"] = wheat
    f["wheat_top2"] = list(wheat.production_loss_mt.sort_values(ascending=False).index[:2])
    partners = c0[~c0.directly_shocked].copy()
    partners["own_share"] = partners.consumption_loss_mt / partners.baseline_use_mt.where(partners.baseline_use_mt > 0)
    big = partners[partners.baseline_use_mt >= PARTNER_MIN_USE]
    small = partners[(partners.baseline_use_mt > 0) & (partners.baseline_use_mt < PARTNER_MIN_USE)]
    f["partner_rows"] = len(partners)
    f["partner_countries"] = partners.iso3.nunique()
    f["partner_big_rows"] = len(big)
    f["partner_big_over1"] = int((big.own_share > 0.01).sum())
    f["partner_big_over5"] = int((big.own_share > 0.05).sum())
    f["partner_big_median"] = float(big.own_share.median())
    f["partner_big_max"] = big.loc[big.own_share.idxmax()]
    f["top_partners"] = big.sort_values("consumption_loss_mt", ascending=False).head(10)
    f["small_max"] = small.loc[small.own_share.idxmax()] if len(small) else None
    for year in (2010, 2011):
        sub = partners[(partners.crop == "wheat") & (partners.year == year)]
        f[f"wheat{year}_partners"] = len(sub)
        f[f"wheat{year}_partner_max"] = sub.loc[sub.consumption_loss_mt.idxmax()]

    # Result 3. Modified PTA, no stocks.
    gp = p0.groupby("crop")
    f["pta_home"] = gp.direct_consumption_loss_mt.sum() / gp.world_consumption_loss_mt.sum()
    f["pta_gross"] = gp.world_consumption_loss_mt.sum()
    f["pta_net"] = -gp.net_consumption_change_mt.sum()
    gains = pc0[(pc0.consumption_loss_mt <= 0)]
    f["pta_converged_rows"] = p0[p0.converged]
    f["pta_capped_rows"] = p0[~p0.converged]

    # By period.
    period = {}
    for label, frame0, frame1 in (
        ("GFD", s0[s0.year <= GFD_LAST], s1[s1.year <= GFD_LAST]),
        ("Sentinel", s0[s0.year > GFD_LAST], s1[s1.year > GFD_LAST]),
    ):
        a = frame0.groupby("crop")
        b = frame1.groupby("crop")
        period[label] = pd.DataFrame({
            "loss": a.production_loss_mt.sum(),
            "home0": a.direct_consumption_loss_mt.sum() / a.world_consumption_loss_mt.sum(),
            "stock_share": b.reserve_draw_mt.sum() / b.production_loss_mt.sum(),
            "remain": -b.net_consumption_change_mt.sum(),
        })
    f["period"] = period

    # Result 4. Stock-to-use.
    psd = reserves[["crop", "iso3", "year", "PSD_Missing_Flag", "stock_to_use", "domestic_use_mt"]]
    own = c1[c1.directly_shocked].merge(psd, on=["crop", "iso3", "year"], how="left")
    own["bin"] = pd.cut(own.stu_network.fillna(0), STU_BINS, labels=STU_LABELS, right=False)
    own["covered"] = np.minimum(own.reserve_draw_mt, own.production_loss_mt)
    own["uncovered"] = own.production_loss_mt - own.covered
    own["coverage"] = own.reserve_draw_mt / own.production_loss_mt.where(own.production_loss_mt > 0)
    f["own"] = own
    f["stu_tbl"] = own.groupby(["crop", "bin"], observed=False)[["production_loss_mt", "uncovered"]].sum()
    f["stu_rows"] = own.groupby("bin", observed=False).size()
    f["stu_full_cover"] = own.groupby("bin", observed=False).coverage.apply(lambda x: (x >= 0.99).mean())
    f["stu_undefined"] = int(own.stu_network.isna().sum())
    undefined = set(map(tuple, own.loc[own.stu_network.isna(), ["crop", "year", "iso3"]].values))
    negative = set(map(tuple, c0.loc[c0.directly_shocked & (c0.baseline_use_mt < -1), ["crop", "year", "iso3"]].values))
    f["stu_undefined_are_negative"] = undefined == negative
    f["uncovered_all"] = own.uncovered.sum()
    low = own.bin.isin(STU_LABELS[:1])
    f["uncovered_low_share"] = own.loc[low, "uncovered"].sum() / own.uncovered.sum()
    f["loss_low_share"] = own.loc[low, "production_loss_mt"].sum() / own.production_loss_mt.sum()
    missing = own.PSD_Missing_Flag.fillna(1).eq(1)
    f["missing_rows"] = int(missing.sum())
    f["missing_share"] = own.loc[missing, "production_loss_mt"].sum() / own.production_loss_mt.sum()
    both = own[own.stu_network.notna() & own.stock_to_use.notna() & (own.baseline_use_mt > 1)]
    f["stu_corr"] = {crop: both[both.crop == crop].stu_network.corr(both[both.crop == crop].stock_to_use) for crop in CROPS}
    f["stu_both_n"] = len(both)
    f["stu_gap_n"] = int(((both.stu_network - both.stock_to_use).abs() > 0.2).sum())

    # Uncertainties and red team.
    own["extra"] = own.reserve_draw_mt - own.production_loss_mt
    extra = own[own.extra > 1]
    f["extra_n"] = len(extra)
    f["extra_t"] = extra.extra.sum()
    f["extra_top"] = extra.loc[extra.extra.idxmax()]
    neg = c0[c0.baseline_use_mt < -1].sort_values(["crop", "year"])
    f["negative_use"] = neg
    f["negative_use_t"] = -neg.baseline_use_mt.sum()
    ratio = own.production_loss_mt / own.beginning_stocks_mt.where(own.beginning_stocks_mt > 0)
    own["loss_to_stock"] = ratio
    f["ratio_q"] = ratio.quantile([0.5, 0.9, 0.99])
    f["ratio_n"] = int(ratio.notna().sum())
    f["zero_stock_rows"] = int((own.beginning_stocks_mt <= 0).sum())
    f["zero_stock_share"] = own.loc[own.beginning_stocks_mt <= 0, "production_loss_mt"].sum() / own.production_loss_mt.sum()
    short = own.beginning_stocks_mt < own.production_loss_mt
    f["short_rows"] = int(short.sum())
    f["short_share"] = own.loc[short, "production_loss_mt"].sum() / own.production_loss_mt.sum()
    prov_own = own[own.year > GFD_LAST]
    f["prov_ratio_median"] = float(prov_own.loss_to_stock.median())
    f["gfd_ratio_median"] = float(own[own.year <= GFD_LAST].loss_to_stock.median())

    # Export share of the shocked countries against the share of the loss that leaves.
    nodes = baseline_nodes(production, trade)
    hit = shocks.merge(nodes, on=["crop", "iso3", "year"], how="left")
    use = hit.use_mt.clip(lower=0)
    hit["export_share"] = (hit.exports_mt / (hit.exports_mt + use)).fillna(0)
    hit["weighted"] = hit.export_share * hit.shock_total_t
    cy = hit.groupby(["crop", "year"]).agg(weighted=("weighted", "sum"), loss=("shock_total_t", "sum")).reset_index()
    cy = cy.merge(s0[["crop", "year", "direct_loss_share"]], on=["crop", "year"])
    cy["export_share"] = cy.weighted / cy.loss
    cy["leaves"] = 1 - cy.direct_loss_share
    f["xs_crop"] = cy.groupby("crop").weighted.sum() / cy.groupby("crop").loss.sum()
    f["leave_crop"] = 1 - f["home0"]
    f["xs_corr"] = float(cy.export_share.corr(cy.leaves))
    f["xs_gap_max"] = float((cy.leaves - cy.export_share).abs().max())
    f["xs_cy"] = cy.set_index(["crop", "year"])

    # Third end member: trade absorbs. Offline, from the network files.
    pool = []
    for (crop, year), block in shocks.groupby(["crop", "year"]):
        here = nodes[(nodes.crop == crop) & (nodes.year == year)]
        rest = here.loc[~here.iso3.isin(block.iso3), "use_mt"].clip(lower=0).sum()
        pool.append({"crop": crop, "year": year, "loss": block.shock_total_t.sum(), "rest_use": rest})
    pool = pd.DataFrame(pool)
    pool["uniform"] = pool.loss / pool.rest_use
    f["pool"] = pool
    f["pool_max"] = pool.loc[pool.groupby("crop").uniform.idxmax()].set_index("crop")
    f["pool_median"] = pool.groupby("crop").uniform.median()
    shares = {}
    for label, frame in (("split1", s1), ("split0", s0), ("pta0", p0)):
        L = frame.production_loss_mt.sum()
        shares[label] = {
            "stocks": frame.reserve_draw_mt.sum() / L,
            "home": frame.direct_consumption_loss_mt.sum() / L,
            "away": (frame.production_loss_mt.sum() - frame.reserve_draw_mt.sum() - frame.direct_consumption_loss_mt.sum()) / L,
        }
    f["em_shares"] = shares

    # Implied response per tonne of harvest loss in the flooded countries, by run.
    implied = {}
    for label, frame in (("Stock-then-split, stocks open", c1), ("Stock-then-split, no stocks", c0), ("Modified PTA, no stocks", pc0)):
        hit = frame[frame.directly_shocked]
        L = hit.production_loss_mt.sum()
        dS = -hit.reserve_draw_mt.sum() / L
        dU = -hit.consumption_loss_mt.sum() / L
        implied[label] = {"stock_change": dS, "domestic_use_mt": dU, "net_imports": 1 + dU + dS}
    end_members = {
        "Stocks absorb": {"stock_change": -1.0, "domestic_use_mt": 0.0, "net_imports": 0.0},
        "Flooded country absorbs": {"stock_change": 0.0, "domestic_use_mt": -1.0, "net_imports": 0.0},
        "Trade absorbs": {"stock_change": 0.0, "domestic_use_mt": 0.0, "net_imports": 1.0},
    }
    f["end_members"] = end_members
    f["implied"] = implied
    observed_check(f, shocks, production, reserves, nodes)
    magnitude(f, shocks, production, reserves, cont)

    # Check that the shock table and the run agree.
    f["identity_prod"] = float((f["prod"] - f["table_total"]).abs().max())

    # Next steps: size of the shocks, a stock-only stress preview, and named cases.
    f["frac_q"] = shocks.frac_of_production.quantile([0.5, 0.9])
    f["frac_over5"] = int((shocks.frac_of_production > 0.05).sum())
    stress = {}
    for k in STRESS_FACTORS:
        scaled = k * own.production_loss_mt
        gap = (scaled - own.beginning_stocks_mt).clip(lower=0)
        stress[k] = {
            "all": gap.sum() / scaled.sum(),
            **{c: gap[own.crop == c].sum() / scaled[own.crop == c].sum() for c in CROPS},
            "rows": int((gap > 0).sum()),
        }
    f["stress"] = stress
    bgd = own[(own.iso3 == "BGD") & (own.crop == "rice_milled")]
    f["bgd_years"] = len(bgd)
    f["bgd_loss"] = bgd.production_loss_mt.sum()
    f["bgd_uncovered"] = bgd.uncovered.sum()
    f["bgd_uncovered_share"] = bgd.uncovered.sum() / own.loc[own.crop == "rice_milled", "uncovered"].sum()
    f["bgd_stu_max"] = float(bgd.stu_network.max())
    f["bgd_short_years"] = sorted(int(y) for y in bgd.loc[bgd.uncovered > 1, "year"])
    bench = []
    for iso, crop, year in BENCHMARKS:
        row = shocks[(shocks.iso3 == iso) & (shocks.crop == crop) & (shocks.year == year)]
        bench.append((iso, crop, year, float(row.shock_total_t.sum()), float(row.frac_of_production.max()) if len(row) else 0.0))
    f["bench"] = bench
    part0 = c0[~c0.directly_shocked]
    low_stu = part0.stu_network.fillna(0) < 0.05
    f["partner_lowstu_share"] = part0.loc[low_stu, "consumption_loss_mt"].sum() / part0.consumption_loss_mt.sum()
    f["partner_loss_alpha1"] = c1.loc[~c1.directly_shocked, "consumption_loss_mt"].sum()
    f["prov_national_mix_share"] = shocks.loc[(shocks.year > GFD_LAST) & (shocks.crop_split == "national production mix"), "shock_total_t"].sum() / shocks.loc[shocks.year > GFD_LAST, "shock_total_t"].sum()
    f["table_crops"] = sorted(shocks.crop.unique())
    return f


# Figures ------------------------------------------------------------------------

def choropleth_row(world, panels, path, colorbar, cmap="OrRd", caption="", norm_power=1.0):
    fig, axes = plt.subplots(1, len(panels), figsize=(4.3 * len(panels), 3.6))
    if len(panels) == 1:
        axes = [axes]
    shared_max = max((float(series[series > 0].max()) for _, series in panels if (series > 0).any()), default=1.0)
    norm = PowerNorm(norm_power, vmin=0, vmax=shared_max)
    for ax, (title, series) in zip(axes, panels):
        frame = series.rename("value").reset_index()
        frame.columns = ["iso3", "value"]
        merged = world.merge(frame, on="iso3", how="left")
        merged.plot(ax=ax, color="#f1f1f1", edgecolor="#d0d0d0", linewidth=0.2)
        show = merged[merged["value"].fillna(0) > 0]
        if len(show):
            show.plot(ax=ax, column="value", cmap=cmap, norm=norm, legend=False)
        ax.set_xlim(-170, 180)
        ax.set_ylim(-58, 78)
        ax.set_axis_off()
        ax.set_title(title, fontsize=10)
    sm = plt.cm.ScalarMappable(cmap=cmap, norm=norm)
    sm._A = []
    cbar = fig.colorbar(sm, ax=axes, fraction=0.02, pad=0.02)
    cbar.set_label(colorbar)
    if caption:
        fig.text(0.01, 0.01, caption, fontsize=7, color="#444444")
    fig.savefig(path, dpi=160, bbox_inches="tight")
    plt.close(fig)


def flow_map(world, edges, countries, crop, year, rule, alpha, title, path, min_cut=2e4):
    pts = world.drop_duplicates("iso3").set_index("iso3")[["lon", "lat"]]
    here = (edges.crop == crop) & (edges.year == year) & (edges.allocation_rule == rule) & (edges.alpha == alpha)
    flows = edges[here & (edges.export_cut_mt >= min_cut)].sort_values("export_cut_mt", ascending=False)
    rises = edges[here & (edges.export_cut_mt <= -min_cut)].sort_values("export_cut_mt").head(15)
    sel = (countries.crop == crop) & (countries.year == year) & (countries.allocation_rule == rule) & (countries.alpha == alpha)
    shocked = set(countries[sel & countries.directly_shocked].iso3)
    losses = countries[sel & (countries.consumption_loss_mt >= min_cut)]
    fig, ax = plt.subplots(figsize=(11.2, 6.0))
    world.plot(ax=ax, color="#f7f7f7", edgecolor="#dddddd", linewidth=0.2)
    if len(losses):
        merged = pts.join(losses.set_index("iso3")["consumption_loss_mt"], how="inner")
        scale = 30 + 380 * np.sqrt(merged.consumption_loss_mt / merged.consumption_loss_mt.max())
        colors = ["#9b2226" if iso in shocked else "#ee9b00" for iso in merged.index]
        ax.scatter(merged.lon, merged.lat, s=scale, c=colors, alpha=0.8, edgecolors="#333333", linewidths=0.3, zorder=3)

    def draw(frame, color, magnitude):
        if frame.empty:
            return
        ceiling = magnitude(frame.export_cut_mt).max()
        for rec in frame.itertuples(index=False):
            if rec.exporter_iso3 not in pts.index or rec.importer_iso3 not in pts.index:
                continue
            x0, y0 = pts.loc[rec.exporter_iso3, ["lon", "lat"]]
            x1, y1 = pts.loc[rec.importer_iso3, ["lon", "lat"]]
            width = 0.4 + 2.2 * (magnitude(rec.export_cut_mt) / ceiling)
            ax.annotate("", xy=(x1, y1), xytext=(x0, y0),
                        arrowprops=dict(arrowstyle="-|>", color=color, lw=width, alpha=0.75, mutation_scale=8), zorder=4)

    draw(flows[flows.exporter_iso3.isin(shocked)].head(25), "#9b2226", lambda v: v)
    draw(flows[~flows.exporter_iso3.isin(shocked)].head(15), "#005f73", lambda v: v)
    draw(rises, "#0a9396", np.abs)
    ax.set_xlim(-170, 180)
    ax.set_ylim(-58, 78)
    ax.set_axis_off()
    ax.set_title(title)
    handles = [
        Line2D([0], [0], color="#9b2226", lw=2, label=f"Export cut by a flooded country, {min_cut / 1e3:.0f} kt or more"),
        Line2D([0], [0], color="#005f73", lw=2, label="Export cut passed on by a partner"),
        Line2D([0], [0], marker="o", color="none", markerfacecolor="#9b2226", markersize=7, label="Use decline in a flooded country"),
        Line2D([0], [0], marker="o", color="none", markerfacecolor="#ee9b00", markersize=7, label="Use decline in a partner"),
    ]
    if not rises.empty:
        handles.append(Line2D([0], [0], color="#0a9396", lw=2, label="Another exporter ships more on an existing link"))
    ax.legend(handles=handles, frameon=False, loc="lower left", fontsize=8)
    fig.text(0.01, 0.01, SOURCE, fontsize=7, color="#444444")
    fig.tight_layout(rect=(0, 0.03, 1, 1))
    fig.savefig(path, dpi=160)
    plt.close(fig)


def make_figures(world, summary, countries, edges, shocks, f):
    FIG.mkdir(parents=True, exist_ok=True)
    written = []
    c0, c1 = pick(countries, SPLIT, 0), pick(countries, SPLIT, 1)
    pc0 = pick(countries, PTA, 0)

    path = FIG / "fig01_production_loss_maps.png"
    panels = [(CROP_LABEL[c], shocks[shocks.crop == c].groupby("iso3").shock_total_t.sum() / 1e6) for c in CROPS]
    choropleth_row(world, panels, path, "Harvest loss, 2002 to 2024 (Mt)", caption=SOURCE + " Shared scale, square root.", norm_power=0.5)
    written.append(path)

    path = FIG / "fig02_annual_losses.png"
    fig, axes = plt.subplots(3, 1, figsize=(9.4, 8.2), sharex=True)
    for ax, crop in zip(axes, CROPS):
        a0 = pick(summary, SPLIT, 0)
        a1 = pick(summary, SPLIT, 1)
        a0 = a0[a0.crop == crop].set_index("year")
        a1 = a1[a1.crop == crop].set_index("year")
        ax.bar(a0.index, a0.production_loss_mt / 1e6, color="#e9d8c4", label="Harvest loss")
        ax.bar(a1.index, a1.reserve_draw_mt / 1e6, color="#94b8c8", width=0.45, label="Drawn from beginning stocks, stocks open")
        ax.plot(a1.index, -a1.net_consumption_change_mt / 1e6, color="#005f73", marker="o", ms=3, label="Remaining world use decline, stocks open")
        ax.set_ylabel("Mt")
        ax.set_title(CROP_LABEL[crop])
        ax.axvline(GFD_LAST + 0.5, color="#888888", lw=0.6, ls="--")
    axes[0].legend(frameon=False, fontsize=8, loc="upper right")
    axes[-1].set_xlabel("Year")
    fig.suptitle("Harvest loss, stock draw, and the world use decline that remains with stocks open")
    fig.text(0.01, 0.005, SOURCE + " Stock-then-split. Dashed line: GFD years to the left, provisional Sentinel-1 years to the right.", fontsize=7, color="#444444")
    fig.tight_layout(rect=(0, 0.03, 1, 0.96))
    fig.savefig(path, dpi=160)
    plt.close(fig)
    written.append(path)

    path = FIG / "fig00_loss_vs_world.png"
    wy = f["world_year"]
    fig, axes = plt.subplots(1, 2, figsize=(10.5, 3.8), sharex=True)
    for crop in CROPS:
        sub = wy.loc[crop]
        axes[0].plot(sub.index, 100 * sub.loss_prod, marker="o", ms=3, color=CROP_COLOR[crop], label=CROP_LABEL[crop])
        axes[1].plot(sub.index, 100 * sub.loss_stock, marker="o", ms=3, color=CROP_COLOR[crop], label=CROP_LABEL[crop])
    axes[0].set_ylabel("Flood loss / world production (%)")
    axes[1].set_ylabel("Flood loss / world beginning stocks (%)")
    for ax in axes:
        ax.axvline(GFD_LAST + 0.5, color="#bbbbbb", lw=0.6, ls="--")
        ax.set_xlabel("Year")
        ax.set_ylim(bottom=0)
    axes[0].legend(frameon=False, fontsize=8)
    fig.suptitle("The applied flood loss each year, against world production and world beginning stocks of the same crop")
    fig.text(0.01, 0.01, SOURCE + " World totals are the 195-country network. Blank USDA stocks count as zero.", fontsize=7, color="#444444")
    fig.tight_layout(rect=(0, 0.05, 1, 0.92))
    fig.savefig(path, dpi=160)
    plt.close(fig)
    written.append(path)

    path = FIG / "fig03_stock_draw_maps.png"
    panels = [(CROP_LABEL[c], c1[c1.crop == c].groupby("iso3").reserve_draw_mt.sum() / 1e6) for c in CROPS]
    choropleth_row(world, panels, path, "Drawn from beginning stocks, 2002 to 2024 (Mt)", cmap="Blues",
                   caption=SOURCE + " Stock-then-split, stocks open. Shared scale, square root.", norm_power=0.5)
    written.append(path)

    path = FIG / "fig04_home_share.png"
    fig, ax = plt.subplots(figsize=(9.4, 4.6))
    for crop in CROPS:
        a = pick(summary, SPLIT, 0)
        a = a[a.crop == crop]
        b = pick(summary, PTA, 0)
        b = b[b.crop == crop]
        ax.plot(a.year, 100 * a.direct_loss_share, marker="o", ms=3, color=CROP_COLOR[crop], label=f"{CROP_LABEL[crop]}, stock-then-split")
        ax.plot(b.year, 100 * b.direct_loss_share, marker="x", ms=3, ls=":", color=CROP_COLOR[crop], alpha=0.6, label=f"{CROP_LABEL[crop]}, modified PTA")
    ax.axhline(100 * HOME_FLOOR, color="#888888", lw=0.6, ls="--")
    ax.axvline(GFD_LAST + 0.5, color="#bbbbbb", lw=0.6, ls="--")
    ax.set_ylim(0, 105)
    ax.set_ylabel("Share of the use decline in flooded countries (%)")
    ax.set_xlabel("Year")
    ax.set_title("Share of the use decline that stays in the flooded countries, no stocks")
    ax.legend(frameon=False, fontsize=7, ncol=2)
    fig.text(0.01, 0.01, f"Horizontal line at {100 * HOME_FLOOR:.0f}%. Modified PTA hit the 50-iteration cap in {f['pta0_cap']} of {f['pta0_n']} crop-years, so its shares are approximate.", fontsize=7, color="#444444")
    fig.tight_layout(rect=(0, 0.05, 1, 1))
    fig.savefig(path, dpi=160)
    plt.close(fig)
    written.append(path)

    for crop in CROPS:
        path = FIG / f"fig05_use_decline_{crop}.png"
        panels = []
        for frame, label in ((c0, "stock-then-split"), (pc0, "modified PTA")):
            series = frame[frame.crop == crop].groupby("iso3").consumption_loss_mt.sum() / 1e6
            panels.append((f"{CROP_LABEL[crop]}, {label}, no stocks", series))
        choropleth_row(world, panels, path, "Use decline, 2002 to 2024 (Mt)",
                       caption=SOURCE + " Both panels share one scale, square root. Declines only.", norm_power=0.5)
        written.append(path)

    path = FIG / "fig06_cover_by_stu.png"
    tbl = f["stu_tbl"]
    fig, axes = plt.subplots(1, 3, figsize=(10.5, 3.8))
    x = np.arange(len(STU_LABELS))
    for ax, crop in zip(axes, CROPS):
        sub = tbl.loc[crop].reindex(STU_LABELS)
        covered = (sub.production_loss_mt - sub.uncovered) / 1e6
        ax.bar(x, covered, color="#94b8c8", label="Covered by own stock draw")
        ax.bar(x, sub.uncovered / 1e6, bottom=covered, color="#9b2226", label="Not covered")
        ax.set_xticks(x)
        ax.set_xticklabels(STU_LABELS, rotation=25, ha="right", fontsize=8)
        ax.set_title(CROP_LABEL[crop])
        ax.set_ylabel("Harvest loss, 2002 to 2024 (Mt)")
    axes[1].legend(frameon=True, fontsize=8, loc="upper left")
    fig.suptitle("Harvest loss by the flooded country's stock-to-use, and how much its own stocks covered")
    fig.text(0.01, 0.01, "Stock-to-use is beginning stocks over network use. Undefined ratios are in the first bin. Stock-then-split, stocks open.", fontsize=7, color="#444444")
    fig.tight_layout(rect=(0, 0.06, 1, 0.92))
    fig.savefig(path, dpi=160)
    plt.close(fig)
    written.append(path)

    path = FIG / "fig07_stu_coverage.png"
    own = f["own"]
    own = own[own.production_loss_mt > 1000]
    fig, axes = plt.subplots(1, 3, figsize=(10.5, 3.8), sharey=True)
    for ax, crop in zip(axes, CROPS):
        sub = own[own.crop == crop]
        ax.scatter(sub.stu_network.fillna(0).clip(upper=1.0), sub.coverage.clip(0, 1.5),
                   s=8 + 40 * np.sqrt(sub.production_loss_mt / sub.production_loss_mt.max()),
                   c="#9b2226", alpha=0.55, edgecolors="none")
        ax.axhline(1, color="#888888", lw=0.5, ls="--")
        ax.set_xlim(-0.02, 1.02)
        ax.set_ylim(-0.05, 1.55)
        ax.set_xlabel("Stock-to-use, capped at 1 for display")
        ax.set_ylabel("Own stock draw / own harvest loss")
        ax.set_title(CROP_LABEL[crop])
    fig.suptitle("Own stock draw divided by own harvest loss, flooded country-years above 1 kt of loss")
    fig.text(0.01, 0.01, "Point size is the harvest loss. Values above 1 mean the country also lost imports. Undefined stock-to-use is drawn at 0.", fontsize=7, color="#444444")
    fig.tight_layout(rect=(0, 0.07, 1, 0.9))
    fig.savefig(path, dpi=160)
    plt.close(fig)
    written.append(path)

    cases = [("wheat", 2010), ("wheat", 2011)]
    for crop in ("rice_milled", "maize"):
        sub = pick(summary, SPLIT, 0)
        sub = sub[sub.crop == crop]
        cases.append((crop, int(sub.loc[sub.abroad_consumption_loss_mt.idxmax(), "year"])))
    for crop, year in cases:
        path = FIG / f"fig08_flow_{crop}_{year}_split.png"
        flow_map(world, edges, countries, crop, year, SPLIT, 0,
                 f"{CROP_LABEL[crop]} {year}: export cuts and use declines, stock-then-split, no stocks", path)
        written.append(path)
    pta_case = ("wheat", 2011)
    path = FIG / f"fig09_flow_{pta_case[0]}_{pta_case[1]}_pta.png"
    flow_map(world, edges, countries, pta_case[0], pta_case[1], PTA, 0,
             f"{CROP_LABEL[pta_case[0]]} {pta_case[1]}: export changes and use declines, modified PTA, no stocks", path)
    written.append(path)

    keep = {p.name for p in written}
    for stale in FIG.glob("*.png"):
        if stale.name not in keep:
            stale.unlink()
    return cases, pta_case


# Report ------------------------------------------------------------------------

def write_markdown(f, cases, pta_case, names):
    L = []
    add = L.append
    fig_no = [0]

    def figure(path, alt, caption):
        fig_no[0] += 1
        add(f"![{alt}](figures/{path})")
        add("")
        add(f"*Figure {fig_no[0]}. {caption}*")
        add("")

    def para(text):
        add(text)
        add("")

    def table(header, rows, numeric=True):
        add("| " + " | ".join(header) + " |")
        add("|" + "|".join("---" if i == 0 or not numeric else "---:" for i in range(len(header))) + "|")
        for row in rows:
            add("| " + " | ".join(str(cell) for cell in row) + " |")
        add("")

    name = lambda iso: short_name(names, iso)

    # Guards for the qualitative sentences.
    require(f["n_runs"] == f["n_rules"] * len(f["alphas"]) * f["n_crop_years"], "grid size")
    require(f["all_in_net"], "every shocked ISO3 is in the network")
    require(f["identity_prod"] < 1, "production loss in the run equals the shock table")
    require(f["split_converged"] == f["split_runs"], "every split run converged")
    require(f["new_links"] == 0, "no new links")
    require(min(f["stock_share"]) > 0.9, "stocks take most of the loss")
    require(min(f["draw_home"]) > 0.9, "the draw is in flooded countries")
    require(min(f["pta_home"]) < 0.5, "modified PTA moves most of the decline out")
    require(set(f["low_years"].crop) == {"wheat"}, "only wheat falls below the home floor")
    require(f["pta0_cap"] == f["pta0_n"] - 1, "one modified PTA run converged")

    s_low = f["low_years"]
    w = f["wheat_year"]
    pr = f["period"]

    add("# Where flood losses of milled rice, wheat, and maize go, 2002 to 2024")
    add("")
    para(f"Input: the draft flood-shock table dated 20 November 2026. Network: annual production, trade, and USDA beginning stocks, {f['year_min']} to {f['year_max']}. Units: metric tonnes (t). 1 kt is a thousand tonnes and 1 Mt is a million tonnes.")

    # 1. Question
    add("## 1. Question")
    add("")
    para("When floods cut the harvest of milled rice, wheat, or maize in 2002 to 2024, does the lost grain show up as lower use in the flooded countries, does it move through trade to other countries, or is it absorbed by beginning stocks? The answer informs whether flood losses should be treated as a local problem, a trade problem, or a stock problem. The result is national commodity availability on this trade network. It is not household consumption, calories, access, or utilization.")
    para("The model cannot observe that answer directly. What it gives is a set of bounds: end-member scenarios that put the whole loss in one place (stocks, the flooded countries, or trade), and two allocation rules that fall between them. Section 4.6 sets out the bounds. Section 5 asks whether observed data can rule any of them out.")

    # 2. Model
    add("## 2. Model")
    add("")
    para("Country node. Each country has production, imports, exports, beginning stocks, and domestic use for one crop and one year. Supply is production plus imports minus exports minus the stock change. A positive stock change is a build, which takes grain out of supply. A negative stock change is a draw, which adds grain to supply. At the start of a run, domestic use equals production plus imports minus exports, so every country is in balance. This report calls that starting value network use.")
    para("Production shock. The simulator receives a negative change in production for each flooded country. If the loss is larger than production, it is clamped so production cannot go below zero. The shortage is network use minus the new supply.")
    para("Alpha. Alpha sets how much of the reported beginning stock may be drawn. At 0 no stock may be drawn. At 1 the whole reported beginning stock may be drawn. This experiment uses only those two values.")
    para("Published PTA. PTA is the proportional trade allocation of the published model. On each pass a country first draws what stock it may, then takes 1 percent of the remaining shortage out of domestic use. The rest is passed through existing trade links: the country cuts its exports and asks for more from the countries it already imports from, in proportion to those flows. In the published code an exporter with no usable stock is frozen and cannot ship more. At alpha 0 every country has no usable stock, so every exporter is frozen and trade only shrinks. When the loop stops, use is reset to supply, floored at zero. This experiment does not use the published default.")
    para(f"Modified PTA. The same update with the empty-reserve freeze turned off, so exporters can increase shipments on links that already exist. An explicit export ban would stay closed. This run has no ban. Because only 1 percent of the shortage comes out of domestic use on each pass, this is the sensitivity in which domestic use is defended and trade carries the loss. With no stocks, {f['pta0_cap']} of {f['pta0_n']} crop-years hit the 50-iteration cap, so partner rankings from those runs are approximate.")
    para("Stock-then-split. Draw stocks first. Split any remaining shortage of at least 1 tonne between an export cut and a domestic-use cut, in proportion to that country's current exports and domestic use. A country whose exports are 20 percent of its exports plus domestic use takes 20 percent of its shortage as an export cut. The export cut is taken pro rata from links that already exist, so the importers on those links now have a shortage of their own. Repeat until every shortage is under 1 tonne. This is the primary rule here. It is not the published default, and it does not open new links or raise any shipment.")
    para("Why two rules can give the same world loss and a different map. Every tonne one country exports is a tonne another imports, so trade cancels in the world sum. World use therefore falls by the harvest loss minus the stock draw, whatever the trade rule. What the rule changes is which countries absorb that fall. The location of the loss is the result that differs between the rules.")
    table(
        ["Rule", "What is drawn", "Domestic use", "Existing link", "New link", "In this run"],
        [
            ["Published PTA", "Stock up to alpha", "1% of the remaining shortage per pass, then reset to supply", "Cut; can rise only from an exporter with usable stock. At alpha 0 cut only", "No", "Not run"],
            ["Modified PTA", "Stock up to alpha", "1% of the remaining shortage per pass, then reset to supply", "Cut or raised", "No", "Sensitivity, alpha 0 and 1"],
            ["Stock-then-split", "Stock up to alpha, first", "Cut by the use share of the shortage", "Cut only", "No", "Primary rule, alpha 0 and 1"],
        ],
        numeric=False,
    )

    # 3. Experiment
    add("## 3. Experiment")
    add("")
    para("Shock. One row of the shock table is one crop, ISO3 country, and calendar year. The applied loss is `shock_total_t`, in tonnes. It already adds the same-year loss from floods in that year and the second-year loss from floods in the previous year. "
         f"Same-year losses are {pct(f['y0_share'])} of the applied tonnes. Rice is milled, so it is not multiplied by 0.67 again.")
    para(f"Coverage. The table has {f['n_rows']:,} crop-country-year rows, {f['n_iso_years']} country-years, {f['n_crop_years']} crop-years, {f['n_years']} years, and {f['n_crops']} crops. It names {f['n_iso3']} countries. "
         + (f"All of them are in the {f['net_size']}-country network." if f["all_in_net"] else "Some are not in the network."))
    para("Scenario. Each crop-year is one scenario. Every flooded country in that crop and year is shocked together, on that year's own production, `trade_mt_net`, and USDA beginning stocks. The 2018 to 2021 mean network is not used.")
    para("Stocks. A blank USDA beginning stock is zero. Imputed stocks are not used. This is not the same stock concept as a cascade that uses the previous market year's ending stocks. A comparison with such a cascade is not a validation until both are run on the same stocks.")
    para(f"Grid. {f['n_rules']} rules by {len(f['alphas'])} values of alpha, {f['n_crop_years']} crop-years each, {f['n_runs']} runs. No price. No export ban.")
    capped = joined(f"{name(r.iso3)} {int(r.year)} {CROP_LOWER[r.crop]} ({tonnes(r.shock_total_t)})" for r in f["capped"].itertuples(index=False))
    para(f"Capped rows. {len(f['capped'])} rows were already capped at production in the table: {capped}.")
    para(f"Flood record. 2002 to 2014 rows come from the GFD record and will not be replaced. 2015 to 2024 rows come from a 1/16 to 1/32 sample of the Sentinel-1 archive. Flooded area in the full archive is larger, so those rows will change. "
         f"The 2015 rows also carry {kt(f['first_prov_y1'])} of second-year loss from 2014 floods. Share of applied tonnes by period:")
    table(
        ["Crop", "2002 to 2014, GFD", "2015 to 2024, Sentinel-1 sample", "Provisional share"],
        [[CROP_LABEL[c], mt(f["gfd_t"][c]), mt(f["prov_t"][c]), pct(f["prov_share"][c])] for c in CROPS]
        + [["All three", mt(f["gfd_t"].sum()), mt(f["prov_t"].sum()), pct(f["prov_share_all"])]],
    )
    top = f["top_shock"]
    tops = "; ".join(
        f"{CROP_LOWER[c]}: " + joined(f"{name(r.iso3)} {mt(r.shock_total_t)}" for r in top[top.crop == c].head(3).itertuples(index=False))
        for c in CROPS
    )
    figure("fig01_production_loss_maps.png", "Harvest loss by country",
           f"Harvest loss is concentrated in a few large producers. The largest totals over 2002 to 2024 are {tops}. All three crops share one scale.")
    mag = f["mag"]
    ml = f["max_loss_prod"]
    para(f"Scale. The tables below set the flood loss against production and USDA beginning stocks in each continent and the world, as annual means over {f['mag_years']} years of the network. Continents follow Natural Earth, which places Russia in Europe. The EU's single USDA stock row is shared across its member countries, as in the network package. Blank stocks count as zero. "
         f"Over 2002 to 2024 the applied flood loss of the three crops together is {pct(f['prod_all'] / f['world_prod_all'], 2)} of their world production. The largest single crop-year is {CROP_LOWER[ml[0][0]]} {ml[0][1]}, at {pct(ml[1], 2)} of world production of that crop.")
    for crop in CROPS:
        sub = mag[mag.crop == crop].set_index("region")
        add(f"{CROP_LABEL[crop]}, mean per year, 2002 to 2024:")
        add("")
        table(
            ["Region", "Production", "Beginning stocks", "Flood loss", "Loss / production", "Loss / beginning stocks"],
            [[r, mt(sub.loc[r, "prod_yr"], 1), mt(sub.loc[r, "stock_yr"], 1), kt(sub.loc[r, "loss_yr"]), pct(sub.loc[r, "loss_prod"], 2), pct(sub.loc[r, "loss_stock"], 2)] for r in ["World"] + CONTINENTS],
        )
    wy = f["world_year"]
    figure("fig00_loss_vs_world.png", "Flood loss against world production and stocks",
           f"Every crop-year's flood loss is a small fraction of world production (at most {pct(wy.loss_prod.max(), 2)}) and of world beginning stocks (at most {pct(wy.loss_stock.max(), 1)}). The dashed line separates the GFD years from the provisional years.")
    add("What was run and what was deliberately not run:")
    add("")
    table(
        ["Item", "Status", "Reason"],
        [
            ["Stock-then-split, alpha 0 and 1", f"Run, {f['split_runs']} runs", "Primary rule"],
            ["Modified PTA, alpha 0 and 1", f"Run, {f['pta0_n'] + f['pta1_n']} runs", "Sensitivity in which domestic use is defended"],
            ["Published PTA with the empty-reserve freeze", "Not run", "At alpha 0 it freezes every exporter"],
            ["RTA (reserves-based trade allocation)", "Not run", "At alpha 0 it has nothing to offer"],
            ["Alpha between 0 and 1", "Not run", "Only no stocks and all reported stocks"],
            ["Imputed stocks for blank USDA rows", "Not used", "Blanks are zero"],
            ["Previous market year's ending stocks", "Not used", "Unshifted beginning stocks only"],
            ["Export bans", "Not run", "Not part of the flood experiment"],
            ["Price, household, or nutrition step", "Not run", "Outside the model"],
            ["2018 to 2021 mean network", "Not used", "Each crop-year runs on its own network"],
        ],
        numeric=False,
    )

    # 4. Results
    add("## 4. Results")
    add("")
    para("Each subsection answers one sentence. A use decline is a fall in a country's domestic use from its network use, counted only where use falls. Home share is the part of the summed use declines that sits in the flooded countries.")

    add("### 4.1 With stocks open, beginning stocks take almost all of the harvest loss, and the draw is in the flooded countries")
    add("")
    para(f"Under stock-then-split at alpha 1, beginning stocks supply {by_crop(f['stock_share'], pct)} of the harvest loss. The remaining world use decline is {by_crop(f['remain'], mt)}, {mt(f['remain_all'])} in all, against a harvest loss of {mt(f['prod_all'])}. "
         f"For scale, that remaining decline is {pct(f['remain_all'] / f['world_prod_all'], 4)} of world production of the three crops over the same years, and the harvest loss drawn from stocks is {pct(f['stock_share_all'] * f['prod_all'] / f['world_stock_crop'].sum(), 2)} of their summed world beginning stocks. "
         f"Of the tonnes drawn from stocks, the share drawn inside the flooded countries is {by_crop(f['draw_home'], pct)}. Partners draw {mt(f['partner_draw_all'], 3)} in all, so they do not replace lost imports from their own stocks in any material quantity.")
    m = f["max_remain_year"]
    para(f"The largest remainder in one crop-year is {CROP_LOWER[m.crop]} {int(m.year)}: {mt(m.rem)} of a {mt(m.production_loss_mt)} harvest loss, with {pct(m.ss)} drawn from stocks.")
    table(
        ["Crop", "Harvest loss", "Drawn from stocks", "Remaining world use decline", "Stock draw inside flooded countries", "Remaining decline inside flooded countries"],
        [[CROP_LABEL[c], mt(f["prod"][c]), pct(f["stock_share"][c]), mt(f["remain"][c]), pct(f["draw_home"][c]), pct(f["remain_home"][c])] for c in CROPS],
    )
    yr1 = f["s1_by_year"]
    n_over90 = int((yr1.ss >= 0.9).sum())
    require(yr1.ss.min() > 0.5, "stocks supply more than half the loss in every crop-year")
    mn1 = f["min_stock_year"]
    figure("fig02_annual_losses.png", "Annual harvest loss and stock draw",
           f"With stocks open, the stock draw (blue bars) supplies at least 90% of the harvest loss (tan bars) in {n_over90} of {len(yr1)} crop-years, and the remaining world use decline (line) stays small. The largest shortfall is {CROP_LOWER[mn1.crop]} {int(mn1.year)}, where stocks supply {pct(mn1.ss)}. The dashed line separates the GFD years from the provisional years.")
    figure("fig03_stock_draw_maps.png", "Stock draw by country",
           f"The stock draw sits in the flooded countries themselves ({by_crop(f['draw_home'], pct)} of the draw), so this map is close to the harvest-loss map in Figure 1.")

    add("### 4.2 With no stocks, most of the decline stays home for milled rice and maize, but not for wheat in 2010 and 2011")
    add("")
    para(f"Under stock-then-split at alpha 0, the home share is {by_crop(f['home0'], pct)}. The world use decline equals the harvest loss within rounding ({by_crop(f['net0'], mt)}).")
    low_text = joined(f"{CROP_LOWER[r.crop]} {int(r.year)} ({pct(r.direct_loss_share)} home, {mt(r.abroad_consumption_loss_mt)} abroad of a {mt(r.production_loss_mt)} harvest loss)" for r in s_low.itertuples(index=False))
    require(sorted(s_low.year) == sorted(f['wheat_top2']), "the low wheat years are the two largest wheat losses")
    para(f"Crop-years in which the home share is below {pct(HOME_FLOOR, 0)}: {low_text}. These are the two largest wheat losses in the table. In those two years the sentence that the loss mostly stays at home fails: close to half the decline is in other countries. The lowest home share for milled rice is {pct(f['min_home0']['rice_milled'])} and for maize {pct(f['min_home0']['maize'])}.")
    wy = f["world_year"]
    para("For scale, the two years' wheat losses are " + joined(f"{pct(wy.loc[('wheat', y), 'loss_prod'], 2)} of world wheat production and {pct(wy.loc[('wheat', y), 'loss_stock'], 2)} of world wheat beginning stocks in {y}" for y in (2010, 2011)) + ".")
    for year in (2010, 2011):
        p = f[f"wheat{year}_partner_max"]
        para(f"In wheat {year}, {f[f'wheat{year}_partners']} partner countries record a use decline above 1 kt. The largest is {name(p.iso3)}, {kt(p.consumption_loss_mt)}, or {pct(p.own_share)} of its own network use.")
    bm = f["partner_big_max"]
    para(f"The transmission is wide and shallow. Across all {f['n_crop_years']} crop-years there are {f['partner_big_rows']:,} partner country-years with at least 0.1 Mt of network use and a decline above 1 kt. Their median decline is {pct(f['partner_big_median'], 2)} of their own use. {f['partner_big_over1']} exceed 1 percent and {f['partner_big_over5']} exceed 5 percent. The highest is {name(bm.iso3)} {CROP_LOWER[bm.crop]} {int(bm.year)}, {pct(bm.own_share)}. The ten largest partner declines in tonnes:")
    table(
        ["Crop", "Year", "Partner", "Use decline", "Share of the partner's own network use"],
        [[CROP_LABEL[r.crop], int(r.year), name(r.iso3), kt(r.consumption_loss_mt), pct(r.own_share)] for r in f["top_partners"].itertuples(index=False)],
    )
    figure("fig04_home_share.png", "Home share by year",
           f"Stock-then-split keeps the home share above {pct(HOME_FLOOR, 0)} in every crop-year except wheat 2010 and 2011. Modified PTA, dotted, puts most of the decline abroad in most years.")
    for crop, year in cases:
        if crop == "wheat":
            claim = f"In wheat {year}, home share {pct(w.loc[year, 'direct_loss_share'])}, export cuts from the flooded countries reach many importers, and each importer loses a small share of its use."
        else:
            row = pick_row(f, crop, year)
            claim = f"The {CROP_LOWER[crop]} year with the most decline abroad, {year}. Home share {pct(row.direct_loss_share)}; {mt(row.abroad_consumption_loss_mt)} abroad of a {mt(row.production_loss_mt)} harvest loss."
        figure(f"fig08_flow_{crop}_{year}_split.png", f"{CROP_LABEL[crop]} {year} flows", claim + " Arrows are cuts on existing links of 20 kt or more. Circles are use declines of 20 kt or more.")

    add("### 4.3 Modified PTA with no stocks moves most of the decline out of the flooded countries")
    add("")
    para(f"Under modified PTA at alpha 0 the home share is {by_crop(f['pta_home'], pct)}, against {by_crop(f['home0'], pct)} under stock-then-split. The world use decline net of gains is the same as the harvest loss ({by_crop(f['pta_net'], mt)}), because trade cancels in the world sum. "
         f"The sum of country declines is larger: {by_crop(f['pta_gross'], mt)}. That gross sum exceeds the harvest loss because some countries gain use while others lose more. It is not the world availability loss.")
    para(f"This is the sensitivity in which domestic use is defended. It is not a second estimate of the same behavior. {f['pta0_cap']} of its {f['pta0_n']} no-stock crop-years stopped at the 50-iteration cap, so which partner loses most is approximate. Section 7 checks whether the disagreement is the rule or the cap.")
    for crop in CROPS:
        figure(f"fig05_use_decline_{crop}.png", f"{CROP_LABEL[crop]} use decline by rule",
               f"{CROP_LABEL[crop]}, no stocks. Stock-then-split keeps {pct(f['home0'][crop])} of the decline in the flooded countries; modified PTA keeps {pct(f['pta_home'][crop])} and spreads the rest. Both panels share one scale.")
    figure(f"fig09_flow_{pta_case[0]}_{pta_case[1]}_pta.png", "Modified PTA flows",
           f"{CROP_LABEL[pta_case[0]]} {pta_case[1]} under modified PTA. Other exporters raise shipments on existing links, so declines appear in countries that were not flooded. Compare with the stock-then-split map of the same year.")

    add("### 4.4 Flooded countries with low stock-to-use are the ones whose stocks do not cover the loss")
    add("")
    para("Stock-to-use is beginning stocks divided by network use, where network use is production plus imports minus exports. Where network use is not positive the ratio is undefined; those country-years are placed in the lowest bin below. "
         f"{f['stu_undefined']} flooded country-years are undefined"
         + (", the same ones that start with negative network use in Section 6." if f["stu_undefined_are_negative"] else "."))
    tbl = f["stu_tbl"]
    rows = []
    for label in STU_LABELS:
        cells = [label, str(int(f["stu_rows"][label]))]
        for c in CROPS:
            cells.append(mt(tbl.loc[(c, label), "production_loss_mt"]))
        cells.append(mt(sum(tbl.loc[(c, label), "uncovered"] for c in CROPS), 3))
        cells.append(pct(f["stu_full_cover"][label]))
        rows.append(cells)
    table(["Stock-to-use", "Flooded country-years", "Milled rice loss", "Wheat loss", "Maize loss", "Not covered by own stocks, all crops", "Country-years with own draw at least 99% of loss"], rows)
    require(min(f['stu_full_cover'][b] for b in STU_LABELS[1:]) > 0.9, "countries above the lowest bin cover their loss")
    require(f['uncovered_low_share'] > 0.5, "misses are in the lowest bin")
    para(f"The relation is a threshold, not a smooth slope. Above a stock-to-use of 0.05 nearly every flooded country covers its own loss, because the loss is small relative to its stocks. The misses are in the lowest bin, which holds {pct(f['loss_low_share'])} of the harvest loss and {pct(f['uncovered_low_share'])} of the tonnes not covered by own stocks ({mt(f['uncovered_all'], 3)} in all).")
    para(f"A missing USDA beginning stock enters this run as zero. Those rows are {f['missing_rows']} of the {f['n_rows']:,} shock rows and carry {pct(f['missing_share'])} of the harvest loss. "
         f"Network stock-to-use and the USDA stock-to-use ratio correlate at {by_crop(f['stu_corr'], lambda v: f'{v:.2f}')}, on {f['stu_both_n']:,} flooded country-years where both are defined. {f['stu_gap_n']} of them differ by more than 0.20.")
    figure("fig06_cover_by_stu.png", "Loss by stock-to-use",
           "Almost all harvest loss falls on countries that cover it from their own stocks. The uncovered tonnes (red) are in the lowest stock-to-use bin.")
    figure("fig07_stu_coverage.png", "Own coverage against stock-to-use",
           "Coverage is a step, not a slope: points sit at 1 once stock-to-use clears a low threshold. The misses are at the left edge.")

    add("### 4.5 The headline does not depend on the provisional 2015 to 2024 rows")
    add("")
    gf, sn = pr["GFD"], pr["Sentinel"]
    table(
        ["Crop", "Harvest loss, GFD", "Harvest loss, Sentinel sample", "No stocks home share, GFD", "No stocks home share, Sentinel sample", "Stocks open drawn share, GFD", "Stocks open drawn share, Sentinel sample"],
        [[CROP_LABEL[c], mt(gf.loss[c]), mt(sn.loss[c]), pct(gf.home0[c]), pct(sn.home0[c]), pct(gf.stock_share[c]), pct(sn.stock_share[c])] for c in CROPS],
    )
    require(gf.stock_share.min() > 0.9 and sn.stock_share.min() > 0.9, "stock share above 90 percent in both periods")
    require((gf.home0 - sn.home0).abs().max() < 0.1, "home share similar in both periods")
    para(f"In the GFD years alone, stocks supply {by_crop(gf.stock_share, pct)} of the harvest loss, and the no-stock home share is {by_crop(gf.home0, pct)}. The provisional years show the same pattern. The applied loss is {by_crop(f['period_loss_prod']['GFD'], lambda v: pct(v, 2))} of world production in the GFD years, against {by_crop(f['period_loss_prod']['Sentinel'], lambda v: pct(v, 2))} in the Sentinel sample. The remaining use decline with stocks open is mostly in the GFD years ({by_crop(gf.remain, mt)}, against {by_crop(sn.remain, mt)} in the Sentinel sample).")

    add("### 4.6 The answer is a range between end members, not a single number")
    add("")
    em = f["em_shares"]
    para("An end member puts the whole harvest loss in one place. Two are already bounded by the runs: with stocks open, stocks take almost all of it; with no stocks and no trade response, the flooded countries would take all of it. The third end member, trade absorbs, is computed offline from the network files and is not a cascade run. It assumes that each flooded country keeps its own use unchanged, that the whole loss is met by imports or by lower exports, that no stocks are drawn, and that the shortfall is shared by every other country in proportion to its positive network use, as in a single fully integrated market. Existing links and their sizes are ignored. Under that assumption every other country loses the same share of its own use.")
    pm, pmax = f["pool_median"], f["pool_max"]
    pool_max_txt = joined(f"{CROP_LOWER[c]} {int(pmax.loc[c, 'year'])} ({pct(pmax.loc[c, 'uniform'], 3)})" for c in CROPS)
    table(
        ["Case", "Taken from stocks", "Use decline in flooded countries", "Use decline elsewhere, net"],
        [
            ["End member: stocks absorb (stock-then-split, stocks open)", pct(em["split1"]["stocks"]), pct(em["split1"]["home"]), pct(em["split1"]["away"])],
            ["End member: flooded countries absorb (offline)", pct(0), pct(1), pct(0)],
            ["Stock-then-split, no stocks", pct(em["split0"]["stocks"]), pct(em["split0"]["home"]), pct(em["split0"]["away"])],
            ["Modified PTA, no stocks", pct(em["pta0"]["stocks"]), pct(em["pta0"]["home"]), pct(em["pta0"]["away"])],
            ["End member: trade absorbs (offline)", pct(0), pct(0), pct(1)],
        ],
    )
    para("Shares are of the total harvest loss over all three crops and all years. "
         f"Under the trade end member, the share of its own use that every non-flooded country loses has a median across crop-years of {by_crop(pm, lambda v: pct(v, 3))}. The largest is {pool_max_txt}. "
         "The largest values are in years when the flooded countries hold most of world use of that crop, so the rest of the world is a small pool. Even the end member that sends everything abroad moves well under one percent of other countries' use in a typical crop-year. The two rules differ in where they sit between the home and trade end members. The stock end member dominates whenever stocks are open.")

    # 5. Observed check
    add("## 5. Can observations tell the end members apart?")
    add("")
    para("Each end member predicts a different observable change in the flooded country in the flood year, per tonne of harvest loss: stocks absorb predicts a stock change of minus one tonne; flooded countries absorb predicts domestic use falling by one tonne; trade absorbs predicts net imports rising by one tonne. The check asks whether observed national data can rule any of them out. It is a falsification test. Agreement with one end member would not confirm the model.")
    para("Data. Production and trade are FAOSTAT, calendar year. Beginning stocks and domestic use are USDA PSD, market year joined to the calendar year on the year number, as in the network package. Stock change is next year's beginning stock minus this year's. Net imports are imports minus exports, from `trade_mt_net`. "
         f"Each series is taken as its change from the previous year. Normal noise is the standard deviation of that change for the same country and crop over 2002 to 2024, leaving out the flood year and the year after, and requiring at least {MIN_NOISE_YEARS} years. Detectability is the harvest loss divided by that noise. Below 1 the flood is smaller than an ordinary year-to-year change; at 2 or more it would stand out.")
    pred_rows = []
    for case_name, pred in list(f["end_members"].items()) + list(f["implied"].items()):
        pred_rows.append([case_name] + [f"{pred[c]:+.2f}" for c in ("stock_change", "domestic_use_mt", "net_imports")])
    add("Predicted change per tonne of harvest loss in the flooded countries:")
    add("")
    table(["Case", "Stock change", "Domestic use", "Net imports"], pred_rows)
    ev = f["obs_events"]
    ev_rows = []
    for r in ev.itertuples(index=False):
        cells = [f"{name(r.iso3)} {r.year} {CROP_LOWER[r.crop]}", kt(r.loss)]
        for col, _ in CHANNELS:
            d, sd = getattr(r, "d_" + col), getattr(r, "sd_" + col)
            cells.append(f"{d / 1e3:+,.0f} kt ({d / sd:+.1f})" if pd.notna(d) and pd.notna(sd) and sd > 0 else "n/a")
        cells.append(f"{r.max_ratio:.2f}")
        ev_rows.append(cells)
    para("Named events. Each cell is the observed change from the previous year, with that change in units of normal noise in brackets. The last column is the largest detectability of the table's loss across the four series.")
    table(["Event", "Table loss"] + [lab for _, lab in CHANNELS] + ["Largest loss / noise"], ev_rows)
    det_ev = ev[ev.max_ratio >= 2]
    det_txt = ""
    event_ruled = []
    for r in det_ev.itertuples(index=False):
        best = max(CHANNELS, key=lambda c: r.loss / getattr(r, "sd_" + c[0]) if getattr(r, "sd_" + c[0]) > 0 else 0)
        out = []
        for em_name, pred in f["end_members"].items():
            for col in ("stock_change", "domestic_use_mt", "net_imports"):
                sd = getattr(r, "sd_" + col)
                if pd.notna(sd) and sd > 0 and abs(getattr(r, "d_" + col) - pred[col] * r.loss) / sd > 2:
                    out.append(em_name)
                    break
        event_ruled.append((r, out))
        det_txt += (f" It is {name(r.iso3)} {r.year} {CROP_LOWER[r.crop]}, where the loss is {r.loss / getattr(r, 'sd_' + best[0]):.1f} times the normal change in {best[1].lower()}. "
                    + (f"By the same two-noise rule, that event is inconsistent with the {joined(chr(34) + n.lower() + chr(34) for n in out)} end member." if out else "By the same two-noise rule, no end member is ruled out."))
    verb = "has" if len(det_ev) == 1 else "have"
    para(f"{len(det_ev)} of the {len(ev)} named events {verb} a table loss at least twice the normal year-to-year change in any series.{det_txt} Several observed changes are large, but the table's flood loss is a small part of them, so they cannot be attributed to the flood. "
         + ("In Pakistan milled rice 2010 and 2022, observed production fell by far more than the table's loss; either other causes dominate those years or the table understates those losses. This check cannot tell which." if all(ev.loc[(ev.iso3 == "PAK") & (ev.crop == "rice_milled"), "d_production_mt"] < -5 * ev.loc[(ev.iso3 == "PAK") & (ev.crop == "rice_milled"), "loss"]) else ""))
    para(f"All rows. Of the {f['det_n']:,} shock rows with enough years to estimate noise, {f['det_prod_1']} have a loss larger than the normal year-to-year change in production and {f['det_prod_2']} have a loss more than twice as large. The median ratio is {f['det_prod_median']:.3f}. Of the {f['det_any_n']:,} rows with all four series, {f['det_any_2']} exceed twice the noise in at least one series.")
    py = f["panel_years"]
    para(f"Pooled test. For every crop and country that floods at least once, regress the year-to-year change in each series on the flood loss in tonnes, {py[0]} to {py[1]}, with a separate mean for each crop-country and standard errors clustered by crop-country. A slope of minus one on stock change is the stocks end member, and so on. A case is ruled out if its prediction is more than two standard errors from the estimated slope in any series.")
    prow = []
    for label, fits in f["pooled"].items():
        prow.append([label] + [f"{fits[c]['b']:+.2f} ± {2 * fits[c]['se']:.2f}" for c, _ in CHANNELS] + [f"{fits['production_mt']['groups']}"])
    table(["Sample", "Production", "Stock change", "Domestic use", "Net imports", "Crop-countries"], prow)
    vrows = []
    for name_, _ in list(f["end_members"].items()) + list(f["implied"].items()):
        cells = [name_]
        for label in f["pooled"]:
            far = f["verdict"][label][name_]
            cells.append("Ruled out (" + ", ".join(dict(CHANNELS)[c].lower() for c in far) + ")" if far else "Not ruled out")
        vrows.append(cells)
    table(["Case"] + list(f["pooled"].keys()), vrows, numeric=False)
    full, trim = f["pooled"]["All flooded crop-countries"], f["pooled"]["Without China and India"]
    prod_ok = abs(full["production_mt"]["b"]) > 2 * full["production_mt"]["se"]
    para(f"Read this with care. The production slope is {full['production_mt']['b']:+.2f} ± {2 * full['production_mt']['se']:.2f}, so "
         + ("the flood loss is visible in production, but only just." if prod_ok else "the flood loss is not clearly visible even in production, and every other slope is conditional on a signal that is not cleanly detected.")
         + " Domestic use in USDA balance sheets is partly a residual of the other items, which ties it to production. "
         + f"Without China and India the production slope shrinks to {trim['production_mt']['b']:+.2f} ± {2 * trim['production_mt']['se']:.2f} and the stock and trade slopes widen (stock change {trim['stock_change']['b']:+.2f} ± {2 * trim['stock_change']['se']:.2f}, net imports {trim['net_imports']['b']:+.2f} ± {2 * trim['net_imports']['se']:.2f}).")
    min_band = min(2 * v["se"] for fits in f["pooled"].values() for v in fits.values())
    require(min_band > 0.8, "pooled bands are wide")
    ruled_full = [n for n, far in f["verdict"]["All flooded crop-countries"].items() if far]
    ruled_trim = [n for n, far in f["verdict"]["Without China and India"].items() if far]
    ev_ruled_txt = " ".join(f"{name(r.iso3)} {r.year} {CROP_LOWER[r.crop]} argues against the {joined(chr(34) + n.lower() + chr(34) for n in out)} end member, but one event at about two noise units is weak evidence." for r, out in event_ruled if out)
    para(f"Verdict. {len(ev) - len(det_ev)} of the {len(ev)} single events cannot rule out any case: their floods are smaller than ordinary year-to-year changes. " + (ev_ruled_txt + " " if ev_ruled_txt else "")
         + (f"Pooled over all flooded crop-countries, the data are inconsistent with {joined(n.lower() for n in ruled_full)}. " if ruled_full else "Pooled over all flooded crop-countries, no case is ruled out. ")
         + (f"Without China and India, only {joined(n.lower() for n in ruled_trim)} remain ruled out. " if ruled_trim else "Without China and India, no case is ruled out. ")
         + ("Any exclusion is weak evidence, sensitive to two countries, and does not choose between the allocation rules. " if ruled_full or ruled_trim else "")
         + f"The two-standard-error ranges are at least plus or minus {min_band:.2f} tonnes per tonne of loss, about the distance between the end members. The result stays a set of bounds, and the observations do not narrow them.")

    # 6. Uncertainties
    add("## 6. Uncertainties, ranked")
    add("")
    para("Ranked by how much of the headline they could move. For each one, the second sentence says what changes if it goes the other way.")
    ext = f["extra_top"]
    neg_list = joined(f"{name(r.iso3)} {CROP_LOWER[r.crop]} {int(r.year)} ({mt(r.baseline_use_mt)})" for r in f["negative_use"].itertuples(index=False))
    items = [
        ("Sentinel-1 sample for 2015 to 2024.",
         f"These rows carry {pct(f['prov_share_all'])} of applied tonnes and are a lower bound on flooded area. They are not comparable with 2002 to 2014 as if the satellite method were the same. "
         f"If full-archive losses are larger, the stock headline holds only while losses stay small relative to stocks; in these rows the median loss is {pct(f['prov_ratio_median'], 2)} of the country's beginning stock. Larger losses would push more country-years into the uncovered case and raise the remaining decline for 2015 to 2024."),
        ("Stock definition.",
         f"This run uses unshifted USDA beginning stocks with blanks as zero. If a cascade uses the previous market year's ending stocks and those are smaller in the flooded countries, the drawn share falls and the remaining decline rises. {pct(f['short_share'])} of the harvest loss is already in country-years whose beginning stock is smaller than the loss, so that is where a smaller stock would bite first. "
         f"Blank stocks carry {pct(f['missing_share'])} of the loss; filling them would raise the drawn share."),
        ("Iteration cap on modified PTA.",
         f"{f['pta0_cap']} of {f['pta0_n']} no-stock crop-years stopped at 50 iterations. World totals do not depend on the cap, because trade cancels. If the runs were continued, partner rankings and home shares could move; the one converged run is in Section 7."),
        ("Network use versus USDA domestic use in stock-to-use.",
         f"{f['stu_gap_n']} of {f['stu_both_n']:,} flooded country-years differ by more than 0.20 between the two ratios. If USDA use were the denominator, some country-years would change bin, but the uncovered tonnes are set by stocks and losses, not by the ratio, so the coverage result would not change."),
        ("Flooded countries that are also importers.",
         f"{f['extra_n']} flooded country-years draw more than their own harvest loss, by {kt(f['extra_t'])} in total, because they also lost imports from other flooded countries. The largest case is {name(ext.iso3)} {CROP_LOWER[ext.crop]} {int(ext.year)}: a harvest loss of {kt(ext.production_loss_mt)} and a draw of {kt(ext.reserve_draw_mt)}. If this were counted as a second copy of the loss, the drawn share would be overstated by at most that amount."),
        ("Negative baseline use.",
         f"{len(f['negative_use'])} flooded country-years start with negative network use, because exports exceed production plus imports: {neg_list}. The split leaves those imbalances in place. The published closer would reset use to supply floored at zero by cutting those countries' exports, {mt(f['negative_use_t'])} in all, and that cut would reach their importers as a loss the flood did not cause. Use declines in this report are computed from use floored at zero, so a negative baseline never shows as a gain."),
        ("No price, no household pass-through, no nutritional conversion.",
         "This run did not compute a model price. If prices respond, a country with small tonnage losses can still face an access problem. Producer prices, food CPI, and the World Bank pink sheet remain the access evidence."),
    ]
    for i, (head, body) in enumerate(items, 1):
        para(f"{i}. {head} {body}")

    # 7. Red team
    add("## 7. Red team")
    add("")
    para("Each attack tries to break the headline. The test, the result, and a verdict follow.")

    para(f"Attack 1. Does \"stocks absorb the loss\" survive in 2002 to 2014 only? Test: stock-then-split at alpha 1, GFD years. Result: drawn share {by_crop(gf.stock_share, pct)}. Verdict: survives.")

    top2 = f["wheat_top2"]
    wtxt = joined(f"{y} ({mt(w.loc[y, 'production_loss_mt'])} loss, {pct(w.loc[y, 'direct_loss_share'])} home)" for y in top2)
    require(all(w.loc[y, 'direct_loss_share'] < HOME_FLOOR for y in top2), "two largest wheat years below the floor")
    para(f"Attack 2. Does \"the loss stays home\" survive for wheat with no stocks? Test: stock-then-split at alpha 0, wheat, all years and the two largest wheat losses. Result: {pct(f['home0']['wheat'])} home over all years; the two largest years are {wtxt}. Verdict: fails for the two largest wheat years. The sentence holds for wheat only as an average that hides them.")

    xs, lv = f["xs_crop"], f["leave_crop"]
    match = f["xs_corr"] > 0.9
    para(f"Attack 3. Is a high home share just the arithmetic of the split? Test: for each crop-year, the flooded countries' exports as a share of exports plus network use, weighted by their loss, against the share of the decline that ends up abroad. Result: export share {by_crop(xs, pct)}; share abroad {by_crop(lv, pct)}. Across the {f['n_crop_years']} crop-years the two correlate at {f['xs_corr']:.2f}, and the largest gap in one crop-year is {100 * f['xs_gap_max']:.1f} percentage points. "
         + ("They match closely. Verdict: the home share is mostly the split arithmetic applied to countries that use most of what they grow. It is a finding about the trade position of the flooded countries under this rule, not about floods. The share abroad sits a little below the export share because part of each export cut lands in another flooded country." if match else "They do not match. Verdict: the home share carries information beyond the split arithmetic."))

    conv = f["pta_converged_rows"]
    if len(conv):
        r = conv.iloc[0]
        capped = f["pta_capped_rows"]
        cw = capped[capped.crop == r.crop]
        split_same = pick_row(f, r.crop, int(r.year))
        para(f"Attack 4. Does modified PTA disagree because the rule differs or because {f['pta0_cap']} runs did not converge? Test: the one no-stock crop-year that converged, {CROP_LOWER[r.crop]} {int(r.year)} ({int(r.n_shocked_countries)} flooded countries, a {kt(r.production_loss_mt)} harvest loss, {int(r.iterations)} iterations). Result: modified PTA home share {pct(r.direct_loss_share)}; the capped {CROP_LOWER[r.crop]} runs range from {pct(cw.direct_loss_share.min())} to {pct(cw.direct_loss_share.max())}, median {pct(cw.direct_loss_share.median())}; stock-then-split in the same year is {pct(split_same.direct_loss_share)}. "
             f"At alpha 1 all {f['pta1_converged']} modified PTA runs converged. "
             + ("Verdict: the converged run sits inside the range of the capped runs and far below the split, so the gap between the rules is the rule, not the cap. One small converged case does not validate the partner ranking of the capped runs."
                if cw.direct_loss_share.min() <= r.direct_loss_share <= cw.direct_loss_share.max() and r.direct_loss_share < split_same.direct_loss_share - 0.2
                else "Verdict: the converged run is further from stock-then-split than any capped run of the same crop, not closer. Running to convergence did not pull modified PTA toward the split, so the disagreement is the rule, not the cap. The capped home shares may be too high rather than too low. One small converged case does not validate the partner ranking of the capped runs."
                if r.direct_loss_share < cw.direct_loss_share.min()
                else "Verdict: the converged run does not look like the capped runs, so part of the disagreement may be the cap."))

    sm = f["small_max"]
    para(f"Attack 5. Are the largest partner percentages an artifact of tiny use? Test: the highest decline as a share of own use among partners with under 0.1 Mt of network use, from the country table, which keeps only declines above 1 kt. Result: {name(sm.iso3)} {CROP_LOWER[sm.crop]} {int(sm.year)}, {kt(sm.consumption_loss_mt, 1)} of {kt(sm.baseline_use_mt, 1)}, {pct(sm.own_share)}. The highest among partners with at least 0.1 Mt is {pct(bm.own_share)}. Verdict: small partners show higher percentages from small tonnages; they are not used to rank impact.")

    pm = f["pta_mass_row"]
    para(f"Attack 6. Do the runs keep mass? Test: world use change minus production change plus stock change, per run. Result: stock-then-split converged in {f['split_converged']} of {f['split_runs']} runs, in at most {f['split_max_iter']} iterations, and closes within {tonnes(f['split_mass_t'])} in every run. Modified PTA's largest gap is {kt(abs(pm.mass_gap_mt), 1)}, on {CROP_LOWER[pm.crop]} in {int(pm.year)} at alpha {pm.alpha:g}. It comes from the closing step that resets use to max(supply, 0): any supply still negative when the loop stops is lifted to zero, which adds grain. "
         "The constant maize residual reported for the older 2018 to 2021 mean benchmark is a different quantity and does not explain this annual-run gap. "
         f"Across all runs there are {f['new_links']} new links, {f['overdraw']} stock overdraws, and {f['negatives']} negative quantities. Verdict: survives.")

    q = f["ratio_q"]
    para(f"Attack 7. Is the stock result an artifact of shocks that are small relative to stocks? Test: each flooded country-year's harvest loss divided by its beginning stock, for the {f['ratio_n']} with a positive stock. Result: median {pct(q[0.5], 2)}, 90th percentile {pct(q[0.9])}, 99th percentile {pct(q[0.99])}. {f['short_rows']} country-years have a stock smaller than the loss; they carry {pct(f['short_share'])} of the tonnes. {f['zero_stock_rows']} have a zero stock, {f['missing_rows']} of them because the USDA row is blank, and carry {pct(f['zero_stock_share'])}. "
         "Verdict: yes, in the sense that matters. Stocks absorb these floods because the floods are small relative to stocks. The result says nothing about a shock several times larger, and should not be extended to one.")

    mn = f["min_stock_year"]
    mh = f["min_remain_home_year"]
    para("Attack 8. What would falsify the lead sentence? The lead is: with beginning stocks open, flood losses are absorbed by stocks, and what remains stays mostly in the flooded countries. It would be falsified by a crop-year in which stocks supply less than half the loss, or in which less than half of the remaining decline is in the flooded countries. "
         f"Result: the lowest drawn share is {CROP_LOWER[mn.crop]} {int(mn.year)}, {pct(mn.ss)}; the lowest home share of the remainder is {CROP_LOWER[mh.crop]} {int(mh.year)}, {pct(mh.direct_loss_share)}. "
         + ("No crop-year falsifies it. " if mn.ss >= 0.5 and mh.direct_loss_share >= 0.5 else "At least one crop-year falsifies it. ")
         + f"The no-stock version of the sentence is already falsified for wheat in {joined(str(int(y)) for y in s_low.year)}.")

    # 8. Next steps
    add("## 8. Next steps")
    add("")
    add("### 8.1 What the model is for at these loss sizes")
    add("")
    fq = f["frac_q"]
    st = f["stress"]
    para(f"The floods in this table are small next to national production and stocks. The median row is {pct(fq[0.5], 2)} of that country's production of the crop, and the 90th percentile is {pct(fq[0.9])}. {f['frac_over5']} of {f['n_rows']:,} rows exceed 5 percent. That is why stocks absorb almost everything when they are open. A run of typical years therefore says little more than the stock arithmetic. The model earns its place where that arithmetic stops working: shocks large enough to exhaust own stocks, several large producers hit in the same year, and importers with thin stocks at the end of the export cuts. The steps below point the model at those cases.")
    para("A first look at where stocks stop working does not need a cascade run. Scale every flooded country's harvest loss by a factor, and count the part that exceeds its own beginning stock. This ignores lost imports, so it is a lower bound on the shortfall a cascade would have to allocate between home use and trade.")
    table(
        ["Loss scaled by", "Share of the scaled loss above own stocks, all crops"] + [CROP_LABEL[c] for c in CROPS] + ["Flooded country-years short"],
        [[f"{k}x", pct(st[k]["all"])] + [pct(st[k][c]) for c in CROPS] + [f"{st[k]['rows']:,} of {len(f['own']):,}"] for k in STRESS_FACTORS],
    )
    require(all(st[k]["rice_milled"] == max(st[k][c] for c in CROPS) and st[k]["wheat"] == min(st[k][c] for c in CROPS) for k in STRESS_FACTORS[:-1]), "rice fastest, wheat slowest")
    require(st[STRESS_FACTORS[-1]]["all"] > 2 * f["uncovered_all"] / f["prod_all"], "stress raises the uncovered share")
    para(f"At the observed size, {pct(f['uncovered_all'] / f['prod_all'])} of the harvest loss is above own stocks. Scaled 10x, the largest crop-year loss would be {pct(10 * f['max_loss_prod'][1], 1)} of world production of that crop. The standard deviation of the year-to-year change in world production of that crop over 2002 to 2024 is {pct(f['world_prod_sd'][f['max_loss_prod'][0][0]], 1)}. Milled rice crosses into the uncovered case fastest and wheat holds out longest. These are arithmetic, not model results.")

    add("### 8.2 Steps, in order")
    add("")
    bench_txt = joined(f"{name(i)} {y} {CROP_LOWER[c]} {kt(t)} ({pct(fr)} of production)" for i, c, y, t, fr in f["bench"])
    short_years = joined(str(y) for y in f["bgd_short_years"])
    steps = [
        f"Replace the 2015 to 2024 rows when the full-archive table arrives, in the same columns, then rerun `Rscript main/run_flood_shocks.R` and `python3 main/plot_flood_report.py`. Do not treat the current maps for those years as final. Larger milled rice losses in South and Southeast Asia would mostly stay at home under the split, for the reason in Attack 3. The case to watch is Bangladesh: across its {f['bgd_years']} flooded milled rice years its stock-to-use never exceeds {f['bgd_stu_max']:.2f}, its own stocks fall short in {short_years}, and it holds {pct(f['bgd_uncovered_share'])} of the uncovered milled rice tonnes.",
        "Attach flood dates before any 2015 to 2024 event is used as a benchmark. The crop split decides which crop network carries the loss, and wheat is the crop where transmission shows. "
        f"In the provisional rows, {pct(f['prov_national_mix_share'])} of tonnes use the national production mix instead of the flooded-cell mix.",
        "Run a stress test on the same flood footprints, scaled 2x, 5x, and 10x, with stock-then-split at alpha 0 and 1. The question is the factor at which own stocks stop covering the loss and the home share starts to fall. Section 8.1 gives the stock arithmetic to check it against. Proposed, not run.",
        "Run compound shocks: several large producers of the same crop in the same year, for example the benchmark events below combined with the largest flood year of another exporter. A single-country flood of the observed size does not test the network.",
        f"Run the benchmark events as named cases. In the current table they are small: {bench_txt}. Pakistan 2022 is a monsoon flood, so its split between wheat and milled rice should be checked against the flood date before it is used. Section 5 shows that these events are mostly smaller than normal year-to-year changes in the observed series, so they illustrate the end members; they do not test them.",
        f"Report impacts for low stock-to-use importers, not only for the flooded countries. With no stocks, partners with stock-to-use under 0.05 take {pct(f['partner_lowstu_share'])} of the partner use decline. With stocks open, partners lose {kt(f['partner_loss_alpha1'])} in all, so this matters only once stocks are exhausted.",
        "Agree one stock definition before comparing with any other cascade, and test intermediate usable-stock shares (alpha 0.25 and 0.5) on it. This run uses unshifted beginning stocks with blanks as zero and only alpha 0 and 1.",
        "Use \"mostly stays at home\" as the lead only for the case with beginning stocks open, and for milled rice and maize with no stocks. Do not use it as the lead for wheat with no stocks.",
        "Decide whether the modified PTA sensitivity should be rerun with a higher iteration cap before any partner ranking from it is quoted. Do not raise the cap inside published `main.R`.",
        "Keep access outside the model. Price responses stay an empirical check, from producer prices, food CPI, and the World Bank pink sheet, until a separate price step is designed. This report does not add one.",
        f"Soybean is in the network package but not in this run; the table used here covers {joined(CROP_LOWER[c] for c in CROPS if c in f['table_crops'])}. Add soybean only when the table carries it.",
    ]
    for i, step in enumerate(steps, 1):
        para(f"{i}. {step}")

    text = "\n".join(L).rstrip() + "\n"
    require("—" not in text and "–" not in text, "no em or en dashes")
    (OUT / "REPORT.md").write_text(text)
    return L


def pick_row(f, crop, year):
    yr = f["_s0"]
    return yr[(yr.crop == crop) & (yr.year == year)].iloc[0]


def plain(text):
    return text.replace("`", "").replace("**", "")


def write_docx(lines):
    doc = Document()
    style = doc.styles["Normal"]
    style.font.name = "Calibri"
    style.font.size = Pt(11)
    table = None
    cols = 0
    for line in lines:
        if line.startswith("|"):
            if set(line.replace("|", "").strip()) <= set("-: "):
                continue
            cells = [plain(part.strip()) for part in line.strip().strip("|").split("|")]
            if table is None:
                table = doc.add_table(rows=1, cols=len(cells))
                table.style = "Table Grid"
                cols = len(cells)
                for i, cell in enumerate(cells):
                    table.rows[0].cells[i].text = cell
                    for run in table.rows[0].cells[i].paragraphs[0].runs:
                        run.bold = True
            elif len(cells) == cols:
                row = table.add_row().cells
                for i, cell in enumerate(cells):
                    row[i].text = cell
            continue
        table = None
        if line.startswith("# "):
            doc.add_heading(plain(line[2:]), level=0)
        elif line.startswith("## "):
            doc.add_heading(plain(line[3:]), level=1)
        elif line.startswith("### "):
            doc.add_heading(plain(line[4:]), level=2)
        elif line.startswith("!["):
            path = OUT / line.split("](")[1].rstrip(")")
            if path.exists():
                doc.add_picture(str(path), width=Inches(6.4))
        elif line.startswith("*") and line.endswith("*"):
            run = doc.add_paragraph().add_run(plain(line.strip("*")))
            run.italic = True
            run.font.size = Pt(9)
        elif line.strip():
            doc.add_paragraph(plain(line))
    doc.save(OUT / "FSC_flood_shock_report.docx")


def main():
    summary, countries, edges, shocks, reserves, production, trade = load_inputs()
    names = names_table()
    world = world_frame()
    cont = continent_map(world, production.iso3.unique())
    facts = build_facts(summary, countries, edges, shocks, reserves, production, trade, cont)
    facts["_s0"] = pick(summary, SPLIT, 0)
    cases, pta_case = make_figures(world, summary, countries, edges, shocks, facts)
    lines = write_markdown(facts, cases, pta_case, names)
    write_docx(lines)
    print(OUT / "REPORT.md")
    print(OUT / "FSC_flood_shock_report.docx")


if __name__ == "__main__":
    main()

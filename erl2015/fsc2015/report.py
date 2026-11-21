"""Figures and a Markdown report for a full run."""

from __future__ import annotations

from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt  # noqa: E402
import numpy as np  # noqa: E402
import pandas as pd  # noqa: E402

from . import config  # noqa: E402

SERIES = ["#2a78d6", "#eb6834"]   # categorical slots 1 and 2: early period, late period
INK, INK2, GRID, SURFACE = "#0b0b0b", "#52514e", "#e4e3df", "#fcfcfb"
CROP_LABEL = {"wheat": "Wheat", "rice": "Rice (milled)"}


def _style(ax):
    ax.set_facecolor(SURFACE)
    for side in ("top", "right"):
        ax.spines[side].set_visible(False)
    for side in ("left", "bottom"):
        ax.spines[side].set_color(INK2)
    ax.tick_params(colors=INK2, labelsize=9)
    ax.grid(True, color=GRID, linewidth=0.8)
    ax.set_axisbelow(True)


def fig_timeseries(ts: pd.DataFrame, path: Path) -> Path:
    fig, axes = plt.subplots(2, 2, figsize=(9, 6), facecolor=SURFACE, sharex=True)
    for col, crop in enumerate(["wheat", "rice"]):
        d = ts[ts["crop"] == crop]
        for row, (key, label) in enumerate([("L", "Trade links, L"), ("G_total_mt", "Global trade, Mt")]):
            ax = axes[row, col]
            _style(ax)
            ax.plot(d["year"], d[key], color=SERIES[0], linewidth=2, marker="o", markersize=3.5)
            ax.set_ylim(bottom=0)
            if row == 0:
                ax.set_title(CROP_LABEL[crop], color=INK, fontsize=11, loc="left")
            if col == 0:
                ax.set_ylabel(label, color=INK2, fontsize=10)
    fig.suptitle("Trade links and volume, FAOSTAT detailed trade matrix", color=INK, fontsize=12, x=0.02, ha="left")
    fig.tight_layout()
    fig.savefig(path, dpi=200)
    plt.close(fig)
    return path


def fig_strength_degree(nodes_by_period: dict, fits: pd.DataFrame, crop: str, path: Path) -> Path:
    fig, ax = plt.subplots(figsize=(5.5, 4.5), facecolor=SURFACE)
    _style(ax)
    for k, (label, nodes) in enumerate(nodes_by_period.items()):
        d = nodes[(nodes["k_out"] > 0) & (nodes["s_out_t"] > 0)]
        fit = fits.set_index("period").loc[label]
        ax.scatter(d["k_out"], d["s_out_t"], s=14, color=SERIES[k], alpha=0.65, linewidths=0,
                   label=f"{label}  (alpha = {fit['alpha']:.2f})")
        kk = np.logspace(np.log10(d["k_out"].min()), np.log10(d["k_out"].max()), 50)
        ax.plot(kk, fit["prefactor_t"] * kk ** fit["alpha"], color=SERIES[k], linewidth=1.5)
    ax.set_xscale("log"); ax.set_yscale("log")
    ax.set_xlabel("Export partners, k_out (period mean)", color=INK2)
    ax.set_ylabel(f"Exports, s_out ({config.UNITS[crop]})", color=INK2)
    ax.set_title(f"{CROP_LABEL[crop]}: export strength vs degree", color=INK, fontsize=11, loc="left")
    ax.legend(frameon=False, fontsize=9, labelcolor=INK2)
    fig.tight_layout()
    fig.savefig(path, dpi=200)
    plt.close(fig)
    return path


def fig_ldc_losses(ldc: dict, path: Path) -> Path:
    """Median LDC loss as a share of staple supply, static vs dynamic, by period."""
    fig, axes = plt.subplots(1, 2, figsize=(9, 3.8), facecolor=SURFACE, sharey=True)
    for ax, (crop, tab) in zip(axes, ldc.items()):
        _style(ax)
        x = np.arange(2)
        width = 0.36
        for k, (_, row) in enumerate(tab.iterrows()):
            vals = [row["static_median_pct"], row["dynamic_median_pct"]]
            bars = ax.bar(x + (k - 0.5) * (width + 0.02), vals, width, color=SERIES[k], label=row["period"])
            for b, v in zip(bars, vals):
                ax.text(b.get_x() + b.get_width() / 2, v, f"{v:.1f}", ha="center", va="bottom", fontsize=8, color=INK2)
        ax.set_xticks(x, ["Static", "Dynamic (GDP order)"])
        ax.set_title(CROP_LABEL[crop], color=INK, fontsize=11, loc="left")
        ax.grid(axis="x", visible=False)
    axes[0].set_ylabel("Median LDC loss, % of staple supply", color=INK2)
    axes[1].legend(frameon=False, fontsize=9, labelcolor=INK2)
    fig.tight_layout()
    fig.savefig(path, dpi=200)
    plt.close(fig)
    return path


# --------------------------------------------------------------------------


def _fmt_table(df: pd.DataFrame, digits: int = 2) -> str:
    def cell(v):
        if isinstance(v, (float, np.floating)):
            return "" if np.isnan(v) else f"{v:,.{digits}f}"
        return str(v)
    head = "| " + " | ".join(map(str, df.columns)) + " |"
    sep = "| " + " | ".join("---" for _ in df.columns) + " |"
    rows = ["| " + " | ".join(cell(v) for v in r) + " |" for r in df.itertuples(index=False)]
    return "\n".join([head, sep, *rows])


METRIC_LABELS = {
    "G_total_mt": "Global trade, Mt/yr",
    "L": "Links",
    "N_out": "Active exporters",
    "N_in": "Active importers",
    "k_out_mean": "Mean export degree",
    "k_in_mean": "Mean import degree",
    "s_out_mean_mt": "Mean export strength, Mt/yr",
    "s_in_mean_mt": "Mean import strength, Mt/yr",
    "k_out_max": "Max export degree",
    "k_in_max": "Max import degree",
    "s_out_max_mt": "Max export strength, Mt/yr",
    "s_in_max_mt": "Max import strength, Mt/yr",
}


def write_report(results: dict, ts: pd.DataFrame, ssr: pd.DataFrame, root: Path = config.OUTPUT_DIR) -> Path:
    root.mkdir(parents=True, exist_ok=True)
    ts.to_csv(root / "timeseries_network.csv", index=False, float_format="%.6g")
    ssr.to_csv(root / "ssr_by_country.csv", index=False, float_format="%.6g")
    fig_timeseries(ts, root / "fig_timeseries.png")
    ldc = {}
    for (crop, scen), r in results.items():
        pn = r.tables["period_nodes"]
        nodes = {lab: pn[pn["period"] == lab] for lab in r.periods}
        fig_strength_degree(nodes, r.tables["power_law"], crop, root / f"fig_strength_degree_{crop}.png")
        ldc[crop] = r.tables["ldc_summary"]
    fig_ldc_losses(ldc, root / "fig_ldc_losses.png")

    periods = next(iter(results.values())).periods
    labels = list(periods)
    lines = [
        "# Python implementation of Puma et al. (2015), updated for recent data",
        "",
        "Puma, M.J., Bose, S., Chon, S.Y., Cook, B.I. (2015). Assessing the evolving fragility of the",
        "global food system. *Environmental Research Letters* 10, 024007. doi:10.1088/1748-9326/10/2/024007",
        "",
        "Generated by `python -m fsc2015 run`. Wheat is in wheat-grain equivalent tonnes and rice in",
        "milled-rice tonnes. Staple supply is FAOSTAT cereals (excluding beer) plus 0.26 times starchy",
        f"roots. Periods: {', '.join(periods)}. Values are means",
        "of the yearly values in each period.",
        "",
        "![Trade links and volume](fig_timeseries.png)",
        "",
    ]
    for (crop, scen), r in results.items():
        t1 = r.tables["table1_network"].copy()
        t1["metric"] = t1["metric"].map(METRIC_LABELS).fillna(t1["metric"])
        t1 = t1[t1["metric"] != "N_active"]
        t2 = r.tables["table2_disturbance"].copy()
        t2["metric"] = t2["metric"].map(METRIC_LABELS).fillna(t2["metric"])
        t2 = t2[t2["metric"] != "N_active"]
        banned = r.tables["bans"]
        lines += [
            f"## {CROP_LABEL[crop]}: export ban `{scen}`",
            "",
            f"Banned exporters with nonzero exports: {int(banned['banned_exporters'].min())} to "
            f"{int(banned['banned_exporters'].max())} per year. LDCs whose imports of this crop exceed their "
            "whole staple supply, and those in `ancillary/ldc_summary_exclusions.csv` (Djibouti, a transit "
            "port), are left out of the LDC rows.",
            "",
            "### Undisturbed network (table 1 analogue)",
            "",
            _fmt_table(t1),
            "",
            "### Losses from the ban, static accounting (table 2 analogue)",
            "",
            _fmt_table(t2),
            "",
            f"![Strength vs degree](fig_strength_degree_{crop}.png)",
            "",
            "### Least developed countries, loss as % of staple supply",
            "",
            _fmt_table(r.tables["ldc_summary"]),
            "",
            "### Largest LDC losses, dynamic accounting (table 3 analogue)",
            "",
        ]
        cp = r.tables["countries_by_period"]
        top_rows = []
        for lab in labels:
            d = cp[(cp["period"] == lab) & cp["ldc"] & ~cp["transit_flag"]].nlargest(5, "dynamic_loss_share")
            for _, row in d.iterrows():
                top_rows.append({"period": lab, "country": row["name"],
                                 "dynamic loss %": 100 * row["dynamic_loss_share"],
                                 "static loss %": 100 * row["static_loss_share"]})
        lines += [_fmt_table(pd.DataFrame(top_rows), 1), ""]
        tops = r.tables["top_exporters"]
        tb = r.tables["top_betweenness"]
        rows = []
        for lab in labels:
            a = tops[tops["period"] == lab].reset_index(drop=True)
            b = tb[tb["period"] == lab].reset_index(drop=True)
            for k in range(min(5, len(a), len(b))):
                rows.append({"period": lab, "rank": k + 1, "exporter": a.loc[k, "name"],
                             "exports, Mt": a.loc[k, "s_out_t"] / 1e6, "betweenness leader": b.loc[k, "name"],
                             "betweenness": b.loc[k, "betweenness"]})
        lines += ["### Top exporters and betweenness, last year of each period", "", _fmt_table(pd.DataFrame(rows), 3), ""]
        notes = r.tables["notes"]
        if len(notes):
            lines += ["Data notes: " + "; ".join(notes["note"]), ""]
    lines += ["![LDC losses](fig_ldc_losses.png)", "", "## Self-sufficiency", ""]
    srows = []
    for lab in labels:
        s = ssr[ssr["period"] == lab]
        srows.append({"period": lab, "countries": len(s),
                      "low or marginal SSR (< 1.05), %": 100 * (s["ssr"] < 1.05).mean(),
                      "median SSR": s["ssr"].median()})
    lines += [_fmt_table(pd.DataFrame(srows), 2), ""]
    path = root / "REPORT.md"
    path.write_text("\n".join(lines))
    return path

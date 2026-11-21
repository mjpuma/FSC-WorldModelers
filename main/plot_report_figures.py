#!/usr/bin/env python3
"""Figures for the FSC flood and export-ban comparison, with layouts in the style of Heslin et al.

Reads country, edge, and network files from main/export_figure_cases.R.
"""

from pathlib import Path

import geopandas as gpd
import matplotlib.pyplot as plt
import networkx as nx
import numpy as np
import pandas as pd
from cartopy.io import shapereader
from matplotlib.lines import Line2D
from matplotlib.patches import Patch

ROOT = Path(__file__).resolve().parents[1]
FIG = ROOT / "outputs" / "flood_benchmarks" / "figures"
OUT = ROOT / "outputs" / "flood_benchmarks"

REGIONS = {
    "Western Africa": ["BEN", "BFA", "CPV", "CIV", "GMB", "GHA", "GIN", "GNB", "LBR", "MLI", "MRT", "NER", "NGA", "SEN", "SLE", "TGO"],
    "Middle Africa": ["AGO", "CMR", "CAF", "TCD", "COD", "COG", "GNQ", "GAB", "STP"],
    "Eastern Africa": ["BDI", "COM", "DJI", "ERI", "ETH", "KEN", "MDG", "MWI", "MUS", "MOZ", "RWA", "SYC", "SOM", "SSD", "UGA", "TZA", "ZMB", "ZWE"],
    "GCC": ["BHR", "KWT", "OMN", "QAT", "SAU", "ARE"],
    "South Asia": ["AFG", "BGD", "BTN", "IND", "IRN", "MDV", "NPL", "PAK", "LKA"],
    "Southeast Asia": ["BRN", "KHM", "IDN", "LAO", "MYS", "MMR", "PHL", "SGP", "THA", "TLS", "VNM"],
}

RICE_CASES = [
    ("rice_IND", "India"),
    ("rice_THA", "Thailand"),
    ("rice_VNM", "Vietnam"),
    ("rice_PAK", "Pakistan"),
]
WHEAT_CASES = [
    ("wheat_RUS", "Russia"),
    ("wheat_EU28", "EU-28"),
    ("wheat_USA", "United States"),
]
COLORS = ["#9b2226", "#bb3e03", "#ca6702", "#ee9b00", "#0a9396", "#005f73"]


def names_table():
    lookup = pd.read_csv(ROOT / "outputs" / "export_for_partners" / "country_lookup_ISO3_FAO.csv")
    return dict(zip(lookup.iso3, lookup.country_name))


def short_name(names, iso):
    name = names.get(iso, iso).split("(")[0].strip()
    return {
        "United States of America": "United States",
        "Russian Federation": "Russia",
        "Cote dIvoire": "Cote d'Ivoire",
        "Viet Nam": "Vietnam",
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


def slice_countries(countries, case_id, rule, alpha, model="PTA"):
    sub = countries[
        (countries.case_id == case_id)
        & (countries.rule == rule)
        & (countries.alpha == alpha)
        & (countries.model == model)
    ]
    return sub


def region_pct(sub, members=None):
    piece = sub if members is None else sub[sub.iso3.isin(members)]
    base = piece.consumption_t0_mt.clip(lower=0).sum()
    loss = piece.consumption_loss_mt.clip(lower=0).sum()
    return 100 * loss / base if base else 0.0


def grouped_loss_bars(countries, cases, panels, title, path):
    fig, axes = plt.subplots(1, len(panels), figsize=(4.15 * len(panels), 5.0), sharey=False)
    if len(panels) == 1:
        axes = [axes]
    labels = list(REGIONS) + ["World"]
    x = np.arange(len(labels))
    width = 0.8 / len(cases)
    for ax, (rule, alpha, panel_title) in zip(axes, panels):
        for i, (case_id, label) in enumerate(cases):
            sub = slice_countries(countries, case_id, rule, alpha)
            heights = []
            for region, members in list(REGIONS.items()) + [("World", None)]:
                heights.append(region_pct(sub, members))
            ax.bar(
                x + (i - (len(cases) - 1) / 2) * width,
                heights,
                width,
                label=label,
                color=COLORS[i % len(COLORS)],
            )
        world = region_pct(slice_countries(countries, cases[0][0], rule, alpha))
        ax.set_xticks(x)
        ax.set_xticklabels(labels, rotation=35, ha="right")
        ax.set_title(f"{panel_title}\n{cases[0][1]} world loss {world:.2f}% of use", fontsize=9)
        ax.set_ylabel("Consumption loss (% of regional use)")
        ax.axhline(0, color="black", lw=0.4)
    axes[-1].legend(frameon=False, fontsize=8, loc="upper right")
    fig.suptitle(title, fontsize=11)
    fig.tight_layout()
    fig.savefig(path, dpi=160)
    plt.close(fig)


def partner_bars(countries, case_id, title, path, names, n=8):
    panels = [
        ("reroute", 0, "PTA, stocks unused, other exporters can reroute"),
        ("frozen", 0, "Stocks unused and every exporter is frozen"),
    ]
    fig, axes = plt.subplots(1, 2, figsize=(11.4, 5.0))
    for ax, (rule, alpha, panel) in zip(axes, panels):
        sub = slice_countries(countries, case_id, rule, alpha)
        shocked = sub.shocked.iloc[0]
        sub = sub[(sub.iso3 != shocked) & (sub.consumption_t0_mt >= 1e5) & (sub.consumption_loss_mt > 0)]
        sub = sub.sort_values("consumption_loss_mt", ascending=False).head(n)
        ylabels = []
        for rec in sub.iloc[::-1].itertuples():
            short = short_name(names, rec.iso3)
            ylabels.append(f"{short} ({100 * rec.loss_fraction:.0f}%)")
        ax.barh(ylabels, sub.consumption_loss_mt[::-1] / 1e6, color="#bb3e03")
        ax.set_xlabel("Consumption loss (Mt). Parentheses are the share of that country's use.")
        ax.set_title(panel, fontsize=9)
    fig.suptitle(title, fontsize=11)
    fig.tight_layout()
    fig.savefig(path, dpi=160)
    plt.close(fig)


def choropleth_pair(world, countries, case_id, panels, title, path, vmax):
    fig, axes = plt.subplots(1, len(panels), figsize=(6.2 * len(panels), 4.2))
    if len(panels) == 1:
        axes = [axes]
    last = None
    for ax, (rule, alpha, model, panel) in zip(axes, panels):
        sub = slice_countries(countries, case_id, rule, alpha, model)[["iso3", "loss_fraction"]]
        merged = world.merge(sub, on="iso3", how="left")
        merged.plot(ax=ax, color="#f1f1f1", edgecolor="#d0d0d0", linewidth=0.2)
        show = merged[merged.loss_fraction > 0.002]
        if len(show):
            last = show.plot(
                ax=ax, column="loss_fraction", cmap="OrRd", vmin=0, vmax=vmax,
                legend=False,
            )
        ax.set_xlim(-170, 180)
        ax.set_ylim(-58, 78)
        ax.set_axis_off()
        ax.set_title(panel, fontsize=9)
    if last is not None:
        sm = plt.cm.ScalarMappable(cmap="OrRd", norm=plt.Normalize(0, vmax))
        sm._A = []
        cbar = fig.colorbar(sm, ax=axes, fraction=0.02, pad=0.01)
        cbar.set_label(f"Share of national use lost (scale capped at {100 * vmax:.0f}%)")
    fig.suptitle(title, fontsize=11)
    fig.subplots_adjust(left=0.06, right=0.90, top=0.86, bottom=0.08, wspace=0.08)
    fig.savefig(path, dpi=160)
    plt.close(fig)


def flow_map(world, countries, edges, case_id, rule, title, path, min_loss=2e5):
    pts = world.drop_duplicates("iso3").set_index("iso3")[["lon", "lat"]]
    flows = edges[
        (edges.case_id == case_id) & (edges.rule == rule) & (edges.alpha == 0) & (edges.model == "PTA")
    ]
    countries_sub = slice_countries(countries, case_id, rule, 0)
    shocked = countries_sub.shocked.iloc[0]
    cuts = flows[(flows.exporter_iso3 == shocked) & (flows.export_cut_mt >= 5e4)]
    rises = flows[(flows.exporter_iso3 != shocked) & (flows.export_cut_mt <= -5e4)]
    losses = countries_sub[countries_sub.consumption_loss_mt >= min_loss]
    fig, ax = plt.subplots(figsize=(11.4, 6.0))
    world.plot(ax=ax, color="#f7f7f7", edgecolor="#dddddd", linewidth=0.2)
    if len(losses):
        merged = pts.join(losses.set_index("iso3")["consumption_loss_mt"], how="inner")
        scale = 40 + 420 * np.sqrt(merged.consumption_loss_mt / merged.consumption_loss_mt.max())
        ax.scatter(
            merged.lon, merged.lat, s=scale, c="#ee9b00", alpha=0.75,
            edgecolors="#9b2226", linewidths=0.4, zorder=3,
        )
    def draw(frame, color):
        if frame.empty:
            return
        ceiling = frame.export_cut_mt.abs().max()
        for rec in frame.itertuples(index=False):
            if rec.exporter_iso3 not in pts.index or rec.importer_iso3 not in pts.index:
                continue
            x0, y0 = pts.loc[rec.exporter_iso3, ["lon", "lat"]]
            x1, y1 = pts.loc[rec.importer_iso3, ["lon", "lat"]]
            width = 0.4 + 2.0 * (abs(rec.export_cut_mt) / ceiling)
            ax.annotate(
                "", xy=(x1, y1), xytext=(x0, y0),
                arrowprops=dict(arrowstyle="-|>", color=color, lw=width, alpha=0.7, mutation_scale=8),
                zorder=4,
            )
    draw(cuts.sort_values("export_cut_mt", ascending=False).head(30), "#9b2226")
    draw(rises.sort_values("export_cut_mt").head(25), "#005f73")
    ax.set_xlim(-170, 180)
    ax.set_ylim(-58, 78)
    ax.set_axis_off()
    ax.set_title(title)
    ax.legend(
        handles=[
            Line2D([0], [0], color="#9b2226", lw=2, label="Banned exporter's cut, ≥ 50 kt"),
            Line2D([0], [0], color="#005f73", lw=2, label="Another exporter ships more, ≥ 50 kt"),
            Line2D([0], [0], marker="o", color="none", markerfacecolor="#ee9b00",
                   markeredgecolor="#9b2226", markersize=8,
                   label=f"Consumption loss ≥ {min_loss / 1e6:.1f} Mt"),
        ],
        frameon=False, loc="lower left", fontsize=8,
    )
    fig.tight_layout()
    fig.savefig(path, dpi=160)
    plt.close(fig)


def baseline_network(world, edges, title, path):
    pts = world.drop_duplicates("iso3").set_index("iso3")[["lon", "lat"]]
    sub = edges[(edges.case_id == "wheat_RUS") & (edges.rule == "reroute") & (edges.alpha == 0)]
    sub = sub.sort_values("trade_t0_mt", ascending=False).drop_duplicates(
        ["exporter_iso3", "importer_iso3"]
    ).head(30)
    fig, ax = plt.subplots(figsize=(11.4, 5.6))
    world.plot(ax=ax, color="#f7f7f7", edgecolor="#dddddd", linewidth=0.2)
    ceiling = sub.trade_t0_mt.max()
    for rec in sub.itertuples(index=False):
        if rec.exporter_iso3 not in pts.index or rec.importer_iso3 not in pts.index:
            continue
        x0, y0 = pts.loc[rec.exporter_iso3, ["lon", "lat"]]
        x1, y1 = pts.loc[rec.importer_iso3, ["lon", "lat"]]
        width = 0.3 + 2.0 * (rec.trade_t0_mt / ceiling)
        ax.annotate(
            "", xy=(x1, y1), xytext=(x0, y0),
            arrowprops=dict(arrowstyle="-|>", color="#005f73", lw=width, alpha=0.7, mutation_scale=8),
        )
    ax.set_xlim(-170, 180)
    ax.set_ylim(-58, 78)
    ax.set_axis_off()
    ax.set_title(title)
    fig.tight_layout()
    fig.savefig(path, dpi=160)
    plt.close(fig)


def circular_network(edges, names, path):
    sub = edges[
        (edges.case_id == "wheat_RUS") & (edges.rule == "reroute") & (edges.alpha == 0) & (edges.model == "PTA")
    ].copy()
    strength = sub.groupby("exporter_iso3").trade_t0_mt.sum().sort_values(ascending=False)
    nodes = list(strength.head(12).index)
    sub = sub[sub.exporter_iso3.isin(nodes) & sub.importer_iso3.isin(nodes)]
    fig, axes = plt.subplots(1, 2, figsize=(11.2, 5.4))
    pos = nx.circular_layout(nodes)
    labels = {iso: short_name(names, iso) for iso in nodes}
    ceiling = max(sub.trade_t0_mt.max(), 1)
    for ax, column, title in (
        (axes[0], "trade_t0_mt", "Baseline wheat flows among the 12 largest exporters"),
        (axes[1], "trade_t1_mt", "Same links after a 95% Russian ban, PTA reroute, no reserve use"),
    ):
        graph = nx.DiGraph()
        graph.add_nodes_from(nodes)
        draw = sub[sub[column] >= 2e5]
        for rec in draw.itertuples(index=False):
            graph.add_edge(rec.exporter_iso3, rec.importer_iso3, w=getattr(rec, column))
        weights = [0.3 + 2.4 * graph[u][v]["w"] / ceiling for u, v in graph.edges]
        nx.draw_networkx_nodes(graph, pos, ax=ax, node_color="#94d2bd", node_size=480, edgecolors="#005f73")
        nx.draw_networkx_labels(graph, pos, labels, ax=ax, font_size=7)
        nx.draw_networkx_edges(
            graph, pos, ax=ax, width=weights, edge_color="#005f73", alpha=0.7,
            arrows=True, arrowsize=8, connectionstyle="arc3,rad=0.08",
        )
        ax.set_title(title, fontsize=9)
        ax.set_axis_off()
    fig.suptitle("Wheat network among the largest exporters. Links below 0.2 Mt are omitted.", fontsize=11)
    fig.tight_layout()
    fig.savefig(path, dpi=160)
    plt.close(fig)


def metrics_bars(metrics, path):
    rows = []
    for case_id, rule, alpha, label in [
        ("wheat_RUS", "frozen", 0, "Wheat, Russia\ntrade frozen"),
        ("wheat_RUS", "reroute", 0, "Wheat, Russia\nPTA reroute"),
        ("wheat_RUS", "frozen", 1, "Wheat, Russia\nreserves open"),
        ("rice_IND", "frozen", 0, "Rice, India\ntrade frozen"),
        ("rice_IND", "reroute", 0, "Rice, India\nPTA reroute"),
        ("rice_IND", "frozen", 1, "Rice, India\nreserves open"),
    ]:
        sub = metrics[(metrics.case_id == case_id) & (metrics.rule == rule) & (metrics.alpha == alpha)].iloc[0]
        rows.append((label, sub.links_t0, sub.links_t1, sub.trade_t0_mt / 1e6, sub.trade_t1_mt / 1e6))
    fig, axes = plt.subplots(1, 2, figsize=(11.2, 4.6))
    x = np.arange(len(rows))
    axes[0].bar(x - 0.15, [r[1] for r in rows], 0.3, label="Baseline links ≥ 1 kt", color="#94d2bd")
    axes[0].bar(x + 0.15, [r[2] for r in rows], 0.3, label="After the shock", color="#005f73")
    axes[1].bar(x - 0.15, [r[3] for r in rows], 0.3, label="Baseline trade", color="#94d2bd")
    axes[1].bar(x + 0.15, [r[4] for r in rows], 0.3, label="After the shock", color="#005f73")
    for ax, ylab in zip(axes, ["Directed links of at least 1 kt", "Total trade (Mt)"]):
        ax.set_xticks(x)
        ax.set_xticklabels([r[0] for r in rows], fontsize=7)
        ax.set_ylabel(ylab)
        ax.legend(frameon=False, fontsize=8)
    fig.suptitle("Full 195-country network. Counts are every link of at least 1 kt, not a sampled edge list.", fontsize=11)
    fig.tight_layout()
    fig.savefig(path, dpi=160)
    plt.close(fig)


def flood_bars(countries, path):
    events = [
        ("thailand_2011", "Thailand rice\n2011 placeholder"),
        ("myanmar_2008", "Myanmar rice\n2008 placeholder"),
        ("pakistan_2022_rice", "Pakistan rice\n2022 placeholder"),
        ("pakistan_2022_wheat", "Pakistan wheat\n2022 placeholder"),
        ("us_midwest_2011_wheat", "US wheat\n2011 placeholder"),
        ("us_midwest_2011_maize", "US maize\n2011 placeholder"),
    ]
    series = [
        ("__production_only", "reroute", 0, "Production loss, trade can reroute", "#ca6702"),
        ("__production_plus_ban", "reroute", 0, "Production loss plus a 95% export ban", "#9b2226"),
        ("__production_plus_ban", "frozen", 1, "Same ban, beginning stocks usable", "#94d2bd"),
    ]
    fig, ax = plt.subplots(figsize=(10.8, 4.8))
    x = np.arange(len(events))
    width = 0.25
    for i, (suffix, rule, alpha, label, color) in enumerate(series):
        heights = []
        for event, _ in events:
            sub = slice_countries(countries, event + suffix, rule, alpha)
            heights.append(sub.consumption_loss_mt.clip(lower=0).sum() / 1e6)
        ax.bar(x + (i - 1) * width, heights, width, label=label, color=color)
    ax.set_xticks(x)
    ax.set_xticklabels([label for _, label in events])
    ax.set_ylabel("World consumption loss (Mt)")
    ax.legend(frameon=False, fontsize=8)
    ax.set_title("Illustrative flood fractions on the 2018-2021 network. These are not observed flood losses.")
    fig.tight_layout()
    fig.savefig(path, dpi=160)
    plt.close(fig)


def loss_location_bars(countries, edges, path):
    specs = [
        ("rice_IND", "reroute", "India rice ban\nreroute"),
        ("rice_IND", "frozen", "India rice ban\ntrade frozen"),
        ("wheat_RUS", "reroute", "Russia wheat ban\nreroute"),
        ("wheat_RUS", "frozen", "Russia wheat ban\ntrade frozen"),
        ("thailand_2011__production_only", "reroute", "Thai rice flood\nreroute"),
        ("us_midwest_2011_maize__production_only", "reroute", "US maize flood\nreroute"),
    ]
    home, substitutes, others = [], [], []
    for case_id, rule, _ in specs:
        sub = slice_countries(countries, case_id, rule, 0)
        shocked = sub.shocked.iloc[0]
        flows = edges[
            (edges.case_id == case_id) & (edges.rule == rule) & (edges.alpha == 0) & (edges.model == "PTA")
        ]
        rose = set(flows[flows.export_cut_mt < -1e3].exporter_iso3) - {shocked}
        loss = sub.set_index("iso3").consumption_loss_mt.clip(lower=0)
        home.append(loss.get(shocked, 0) / 1e6)
        substitutes.append(loss.reindex(list(rose)).fillna(0).sum() / 1e6)
        others.append((loss.sum() - loss.get(shocked, 0) - loss.reindex(list(rose)).fillna(0).sum()) / 1e6)
    fig, ax = plt.subplots(figsize=(10.6, 4.8))
    x = np.arange(len(specs))
    ax.bar(x, home, 0.7, label="Shocked country", color="#9b2226")
    ax.bar(x, substitutes, 0.7, bottom=home, label="Other countries that export more", color="#005f73")
    bottom2 = np.array(home) + np.array(substitutes)
    ax.bar(x, others, 0.7, bottom=bottom2, label="Everyone else", color="#ee9b00")
    ax.set_xticks(x)
    ax.set_xticklabels([s[2] for s in specs])
    ax.set_ylabel("World consumption loss (Mt)")
    ax.legend(frameon=False, fontsize=8)
    ax.set_title("Who eats less. A pure export ban holds the grain as the exporter's stock build, so the shocked country's own use barely changes.")
    fig.tight_layout()
    fig.savefig(path, dpi=160)
    plt.close(fig)


def fmt_pct(countries, case_id, rule, alpha, members=None, model="PTA"):
    return region_pct(slice_countries(countries, case_id, rule, alpha, model), members)


def top_line(countries, case_id, rule, names, n=5, min_use=1e5):
    sub = slice_countries(countries, case_id, rule, 0)
    shocked = sub.shocked.iloc[0]
    sub = sub[(sub.iso3 != shocked) & (sub.consumption_t0_mt >= min_use) & (sub.consumption_loss_mt > 0)]
    sub = sub.sort_values("consumption_loss_mt", ascending=False).head(n)
    return ", ".join(
        f"{short_name(names, r.iso3)} {r.consumption_loss_mt / 1e6:.2f} Mt ({100 * r.loss_fraction:.0f}% of use)"
        for r in sub.itertuples()
    )


def write_report(countries, balance, metrics, names):
    rus_world_frozen = fmt_pct(countries, "wheat_RUS", "frozen", 0)
    rus_world_open = fmt_pct(countries, "wheat_RUS", "frozen", 1)
    ind_gcc_frozen = fmt_pct(countries, "rice_IND", "frozen", 0, REGIONS["GCC"])
    ind_gcc_reroute = fmt_pct(countries, "rice_IND", "reroute", 0, REGIONS["GCC"])
    ind_world = fmt_pct(countries, "rice_IND", "reroute", 0)
    maize_open = fmt_pct(countries, "maize_USA", "frozen", 1)
    maize_closed = fmt_pct(countries, "maize_USA", "reroute", 0)
    us_flood = slice_countries(countries, "us_midwest_2011_maize__production_only", "reroute", 0)
    us_flood_mt = us_flood.consumption_loss_mt.clip(lower=0).sum() / 1e6
    us_both = slice_countries(countries, "us_midwest_2011_maize__production_plus_ban", "reroute", 0)
    us_both_mt = us_both.consumption_loss_mt.clip(lower=0).sum() / 1e6
    gap = balance.loc[balance.identity_gap_mt.abs().idxmax()]

    wheat_m = metrics[(metrics.case_id == "wheat_RUS") & (metrics.rule == "reroute") & (metrics.alpha == 0)].iloc[0]
    lines = [
        "# FSC flood and export-ban results on the 2018-2021 mean network",
        "",
        "The runs use the Food Shock Cascade on 2018-2021 mean networks, in milled-rice, wheat, and maize metric tons. A 95% export ban removes that share of the named exporter's flows and holds the grain as a stock build. Alpha is the share of USDA beginning stocks that can be drawn. PTA is proportional trade allocation. RTA is reserves-based trade allocation, with exporters offering 20% of remaining usable reserves.",
        "",
        "## What these figures are",
        "",
        "The panels compare export bans and illustrative flood fractions. They are availability results for one commodity. They are not observed flood losses, household consumption, prices, or nutritional outcomes. An observed country-year flood table is not in this repository.",
        "",
        "The uncalibrated rice price index in `benchmark_results.csv` is left out of the figures. It is not a price forecast.",
        "",
        "## Reserves, and two different meanings of no reserves",
        "",
        f"A 95% Russian wheat ban removes {rus_world_frozen:.2f}% of world wheat use when stocks cannot be drawn. Opening all beginning stocks brings that to {rus_world_open:.2f}%. An Indian rice ban removes {ind_world:.2f}% of world rice use either way the trade rule is set, because the world total is the grain held off the market. Where that loss lands depends on the trade rule.",
        "",
        f"FSC's published setting freezes the exports of every country whose usable reserves are zero. At alpha = 0 that is every country, so partners who bought from the banned exporter cannot be resupplied. GCC rice use falls by {ind_gcc_frozen:.0f}% under an Indian ban. PTA can instead leave stocks unused and still shift volume along existing links. Under that rule the same Indian ban cuts GCC rice use by {ind_gcc_reroute:.0f}%, and the tonnes show up as lower consumption in Thailand, Vietnam, Pakistan, China, and the United States, the exporters that ship more to fill the gap. RTA with no reserves tracks the frozen case: it can offer grain only out of reserves, and there are none to offer.",
        "",
        "The three panels use separate vertical scales. The reserves panel is a small number, and a shared scale hides it.",
        "",
        "![Rice bans under three trade rules](figures/fig1_rice_reserves_vs_none.png)",
        "",
        "![Wheat bans under three trade rules](figures/figA1_wheat_reserves_vs_none.png)",
        "",
        f"A 95% US maize ban is the reserve case that still leaves a visible world loss: {maize_closed:.2f}% of world maize use with stocks unused, and {maize_open:.2f}% with beginning stocks fully usable.",
        "",
        "![US maize ban](figures/figA2_maize_usa.png)",
        "",
        "## Who is affected",
        "",
        "Partners are ranked by tonnes, among countries with at least 0.1 Mt of baseline use. A ranking by percentage puts markets such as the Faroe Islands, whose entire rice use is about 155 tonnes, at the top of the list.",
        "",
        "India rice ban, other exporters can reroute: " + top_line(countries, "rice_IND", "reroute", names) + ".",
        "",
        "India rice ban, trade frozen: " + top_line(countries, "rice_IND", "frozen", names) + ".",
        "",
        "![Partners after an Indian rice ban](figures/fig1c_rice_india_partners.png)",
        "",
        "Russia wheat ban, other exporters can reroute: " + top_line(countries, "wheat_RUS", "reroute", names) + ".",
        "",
        "Russia wheat ban, trade frozen: " + top_line(countries, "wheat_RUS", "frozen", names) + ".",
        "",
        "![Partners after a Russian wheat ban](figures/fig1c_wheat_russia_partners.png)",
        "",
        "![Where the lost consumption sits](figures/fig_where_loss_sits.png)",
        "",
        "FSC takes only 1% of the remaining shortage out of domestic use on each iteration and pushes the rest into trade. Under the reroute setting, the Thai rice placeholder, a 15% production cut, removes about 3.2 Mt of world rice use and only a small share of that is Thai consumption. Published PTA with the freeze on does not do that for every exporter. The comparison table records the shocked country's share. These placeholder maps follow FSC. The stock-then-split option is a separate run on the observed draft table.",
        "",
        "## Where the grain travels",
        "",
        "Red arrows are the banned exporter's cuts of at least 50 kt. Teal arrows are increases of at least 50 kt on some other exporter's links. Orange circles are the larger consumption losses (0.2 Mt for rice, 0.8 Mt for wheat). The layout follows the route maps in Heslin et al., drawn on the 2018-2021 mean network.",
        "",
        "![Indian rice routes when other exporters can reroute](figures/fig2_rice_india_network.png)",
        "",
        "![Russian wheat routes when other exporters can reroute](figures/fig2_wheat_russia_network.png)",
        "",
        "The choropleths use one color scale. The left map is PTA with rerouting and no reserve use. The right map is RTA with no reserve use, which leaves the loss on the importers. Countries above the cap are drawn at the cap.",
        "",
        "![Wheat consumption loss, PTA reroute and RTA](figures/fig_heslin_wheat_russia_choropleth.png)",
        "",
        "![Rice consumption loss, PTA reroute and RTA](figures/fig_heslin_rice_india_choropleth.png)",
        "",
        "![Largest baseline wheat flows](figures/fig_heslin_wheat_baseline_network.png)",
        "",
        "![Wheat links among the twelve largest exporters](figures/fig_heslin_wheat_network_diagram.png)",
        "",
        f"The wheat network has {int(wheat_m.links_t0)} directed links of at least 1 kt and {wheat_m.trade_t0_mt / 1e6:.0f} Mt of trade. After the Russian ban with rerouting, {int(wheat_m.links_t1)} links remain and trade is {wheat_m.trade_t1_mt / 1e6:.0f} Mt. Freezing every exporter, which is what alpha = 0 does in the published code, drops the wheat link count from {int(wheat_m.links_t0)} to "
        + f"{int(metrics[(metrics.case_id == 'wheat_RUS') & (metrics.rule == 'frozen') & (metrics.alpha == 0)].iloc[0].links_t1)}.",
        "",
        "![Network size before and after the bans](figures/fig_heslin_network_metrics.png)",
        "",
        "## Placeholder floods",
        "",
        f"The US maize placeholder is a 10% cut in US production, about {us_flood_mt:.1f} Mt of world consumption when stocks are unused. Adding a 95% export ban raises the world consumption loss to {us_both_mt:.1f} Mt, because the ban holds a further block of exports as stocks. With beginning stocks usable, the production-only case is almost fully covered and the ban still leaves the same residue as the pure US maize ban. Replace `inputs/placeholder_flood_shocks.csv` before reading this panel as history. The US shock hits the whole United States node.",
        "",
        "![Placeholder flood losses](figures/fig3_placeholder_floods.png)",
        "",
        "## Mass balance",
        "",
        "World trade cancels. The change in world consumption equals the production change minus the net stock build, aside from any country whose domestic supply would have gone negative and was clipped to zero. A pure export ban has no production change. Its consumption loss is the stock build, the grain the exporter held back.",
        "",
        "| Case | Rule | Alpha | Consumption lost (Mt) | Consumption gained (Mt) | Production change (Mt) | Stock change (Mt) | Gap (Mt) |",
        "|---|---|---:|---:|---:|---:|---:|---:|",
    ]
    keep = balance[
        balance.case_id.isin([
            "wheat_RUS", "rice_IND", "maize_USA",
            "us_midwest_2011_maize__production_only",
            "us_midwest_2011_maize__production_plus_ban",
        ])
        & balance.ok
    ].sort_values(["case_id", "rule", "alpha"])
    for rec in keep.itertuples(index=False):
        lines.append(
            f"| {rec.case_id} | {rec.rule} | {rec.alpha:g} | {rec.consumption_loss_mt / 1e6:.2f} | "
            f"{rec.consumption_gain_mt / 1e6:.2f} | {rec.production_change_mt / 1e6:.2f} | "
            f"{rec.stock_change_mt / 1e6:.2f} | {rec.identity_gap_mt / 1e6:.3f} |"
        )
    lines += [
        "",
        f"The largest identity gap is {gap.identity_gap_mt / 1e6:.2f} Mt, on {gap.case_id} under the {gap.rule} rule at alpha {gap.alpha:g}. Every maize run shares a gap of about 0.12 Mt, including the production shock that reserves cover completely, so that residual is in the baseline maize balance. The wheat and rice headline runs sit within 1 kt.",
        "",
        "## Red team",
        "",
        "- Alpha 0 closes reserves and, in published PTA, also freezes every export row. The frozen panels are that published rule. The reroute panels turn the freeze off.",
        "- World percentages under a ban are the size of the stock build. They stay the same when trade reroutes. The maps are what change.",
        "- Rankings use tonnes and a 0.1 Mt use floor. Percentage losses in very small markets are real on this network and are a poor guide to where the grain went.",
        "- FSC's 1% domestic-absorption step passes a production shock out through exports under the reroute setting. The placeholder flood maps follow FSC. Stock-then-split is a separate option and is the primary rule for the observed draft table.",
        "- The rice price index is uncalibrated and is not a Pink Sheet result.",
        "- EU-28 follows the existing FSC member list and includes the United Kingdom.",
        "- Losses are shares of that commodity's use. They are not shares of calories or of a household food basket.",
        "- A historical flood series needs an observed country-year loss table. The year on a shock row stays metadata while these benchmarks use the 2018-2021 mean.",
        "",
    ]
    (OUT / "REPORT.md").write_text("\n".join(lines))


def main():
    countries = pd.read_csv(FIG / "country_outcomes.csv")
    edges = pd.read_csv(FIG / "edge_outcomes.csv")
    balance = pd.read_csv(FIG / "mass_balance.csv")
    metrics = pd.read_csv(FIG / "network_metrics.csv")
    names = names_table()
    world = world_frame()
    missing = set(countries.loc[countries.consumption_loss_mt > 5e5, "iso3"]) - set(world.iso3)
    if missing:
        print("ISO codes with large losses missing from the map:", sorted(missing))
    plt.rcParams.update({"font.size": 9, "axes.spines.top": False, "axes.spines.right": False})
    panels = [
        ("frozen", 0, "No reserves, trade frozen"),
        ("reroute", 0, "No reserves, PTA can reroute"),
        ("frozen", 1, "Beginning stocks usable"),
    ]
    grouped_loss_bars(
        countries, RICE_CASES, panels,
        "95% rice export ban on the 2018–2021 mean network. Panel scales differ.",
        FIG / "fig1_rice_reserves_vs_none.png",
    )
    grouped_loss_bars(
        countries, WHEAT_CASES, panels,
        "95% wheat export ban on the 2018–2021 mean network. Panel scales differ.",
        FIG / "figA1_wheat_reserves_vs_none.png",
    )
    grouped_loss_bars(
        countries, [("maize_USA", "United States")], panels,
        "95% US maize export ban. Panel scales differ.",
        FIG / "figA2_maize_usa.png",
    )
    partner_bars(
        countries, "rice_IND",
        "Largest rice-consumption losses after a 95% Indian export ban. Countries using under 0.1 Mt are omitted.",
        FIG / "fig1c_rice_india_partners.png", names,
    )
    partner_bars(
        countries, "wheat_RUS",
        "Largest wheat-consumption losses after a 95% Russian export ban. Countries using under 0.1 Mt are omitted.",
        FIG / "fig1c_wheat_russia_partners.png", names,
    )
    flow_map(
        world, countries, edges, "rice_IND", "reroute",
        "Indian rice ban, no reserve use, other exporters can reroute",
        FIG / "fig2_rice_india_network.png",
    )
    flow_map(
        world, countries, edges, "wheat_RUS", "reroute",
        "Russian wheat ban, no reserve use, other exporters can reroute",
        FIG / "fig2_wheat_russia_network.png",
        min_loss=8e5,
    )
    pair = [
        ("reroute", 0, "PTA", "PTA, stocks unused, trade can reroute"),
        ("rta", 0, "RTA", "RTA, stocks unused"),
    ]
    choropleth_pair(
        world, countries, "wheat_RUS", pair,
        "Wheat consumption loss after a 95% Russian export ban",
        FIG / "fig_heslin_wheat_russia_choropleth.png", vmax=0.35,
    )
    choropleth_pair(
        world, countries, "rice_IND", pair,
        "Rice consumption loss after a 95% Indian export ban",
        FIG / "fig_heslin_rice_india_choropleth.png", vmax=0.25,
    )
    baseline_network(
        world, edges,
        "Largest 2018–2021 mean wheat flows",
        FIG / "fig_heslin_wheat_baseline_network.png",
    )
    circular_network(edges, names, FIG / "fig_heslin_wheat_network_diagram.png")
    metrics_bars(metrics, FIG / "fig_heslin_network_metrics.png")
    flood_bars(countries, FIG / "fig3_placeholder_floods.png")
    loss_location_bars(countries, edges, FIG / "fig_where_loss_sits.png")
    write_report(countries, balance, metrics, names)
    print("Wrote", OUT / "REPORT.md")


if __name__ == "__main__":
    main()

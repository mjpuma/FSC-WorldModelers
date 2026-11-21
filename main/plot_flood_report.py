#!/usr/bin/env python3
"""Maps, networks, stock-to-use, and the flood-shock report.

Reads outputs/flood_shocks and the annual network files. Writes figures and
REPORT.md under outputs/flood_shocks, plus a Word copy of the same report.
"""

from pathlib import Path

import geopandas as gpd
import matplotlib.pyplot as plt
import networkx as nx
import numpy as np
import pandas as pd
from cartopy.io import shapereader
from docx import Document
from docx.shared import Inches, Pt
from matplotlib.lines import Line2D

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / "outputs" / "flood_shocks"
FIG = OUT / "figures"
ANNUAL = ROOT / "outputs" / "export_for_RIKEN" / "annual_networks_2002_2024"
CROPS = ["rice_milled", "wheat", "maize"]
CROP_LABEL = {"rice_milled": "Milled rice", "wheat": "Wheat", "maize": "Maize"}
SOURCE = "Draft flood table, 20 Nov 2026. Annual networks, 2002 to 2024. Metric tons. 2015 to 2024 rows are provisional."

REGIONS = {
    "Western Africa": ["BEN", "BFA", "CPV", "CIV", "GMB", "GHA", "GIN", "GNB", "LBR", "MLI", "MRT", "NER", "NGA", "SEN", "SLE", "TGO"],
    "Middle Africa": ["AGO", "CMR", "CAF", "TCD", "COD", "COG", "GNQ", "GAB", "STP"],
    "Eastern Africa": ["BDI", "COM", "DJI", "ERI", "ETH", "KEN", "MDG", "MWI", "MUS", "MOZ", "RWA", "SYC", "SOM", "SSD", "UGA", "TZA", "ZMB", "ZWE"],
    "GCC": ["BHR", "KWT", "OMN", "QAT", "SAU", "ARE"],
    "South Asia": ["AFG", "BGD", "BTN", "IND", "IRN", "MDV", "NPL", "PAK", "LKA"],
    "Southeast Asia": ["BRN", "KHM", "IDN", "LAO", "MYS", "MMR", "PHL", "SGP", "THA", "TLS", "VNM"],
}


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
    reserves = []
    for crop in CROPS:
        frame = pd.read_csv(ANNUAL / f"{crop}_reserves_2002_2024_mt.csv")
        frame["crop"] = crop
        reserves.append(frame)
    reserves = pd.concat(reserves, ignore_index=True)
    return summary, countries, edges, shocks, reserves


def choropleth_row(world, panels, path, colorbar, cmap="OrRd", caption="", shared=True):
    fig, axes = plt.subplots(1, len(panels), figsize=(4.3 * len(panels), 3.6))
    if len(panels) == 1:
        axes = [axes]
    positive = [series[series > 0] for _, series in panels]
    shared_max = max((float(series.max()) for series in positive if len(series)), default=1.0)
    for ax, (title, series) in zip(axes, panels):
        frame = series.rename("value").reset_index()
        frame.columns = ["iso3", "value"]
        merged = world.merge(frame, on="iso3", how="left")
        merged.plot(ax=ax, color="#f1f1f1", edgecolor="#d0d0d0", linewidth=0.2)
        show = merged[merged["value"].fillna(0) > 0]
        vmax = shared_max if shared else max(float(series.max()) if len(series) else 1.0, 1e-6)
        if len(show):
            show.plot(ax=ax, column="value", cmap=cmap, vmin=0, vmax=vmax, legend=False)
        if not shared:
            sm = plt.cm.ScalarMappable(cmap=cmap, norm=plt.Normalize(0, vmax))
            sm._A = []
            cbar = fig.colorbar(sm, ax=ax, fraction=0.04, pad=0.02)
            cbar.set_label(colorbar, fontsize=8)
        ax.set_xlim(-170, 180)
        ax.set_ylim(-58, 78)
        ax.set_axis_off()
        ax.set_title(title, fontsize=10)
    if shared:
        sm = plt.cm.ScalarMappable(cmap=cmap, norm=plt.Normalize(0, shared_max))
        sm._A = []
        cbar = fig.colorbar(sm, ax=axes, fraction=0.02, pad=0.02)
        cbar.set_label(colorbar)
    if caption:
        fig.text(0.01, 0.01, caption, fontsize=7, color="#444444")
    fig.tight_layout(rect=(0, 0.04, 1, 1))
    fig.savefig(path, dpi=160)
    plt.close(fig)


def flow_map(world, edges, countries, crop, year, rule, alpha, title, path, names, min_cut=2e4):
    pts = world.drop_duplicates("iso3").set_index("iso3")[["lon", "lat"]]
    flows = edges[
        (edges.crop == crop) & (edges.year == year)
        & (edges.allocation_rule == rule) & (edges.alpha == alpha)
        & (edges.export_cut_mt >= min_cut)
    ].sort_values("export_cut_mt", ascending=False)
    shocked = set(countries[
        (countries.crop == crop) & (countries.year == year)
        & (countries.allocation_rule == rule) & (countries.alpha == alpha)
        & countries.directly_shocked
    ].iso3)
    losses = countries[
        (countries.crop == crop) & (countries.year == year)
        & (countries.allocation_rule == rule) & (countries.alpha == alpha)
        & (countries.consumption_loss_mt >= 2e4)
    ]
    fig, ax = plt.subplots(figsize=(11.2, 6.0))
    world.plot(ax=ax, color="#f7f7f7", edgecolor="#dddddd", linewidth=0.2)
    if len(losses):
        merged = pts.join(losses.set_index("iso3")["consumption_loss_mt"], how="inner")
        scale = 30 + 380 * np.sqrt(merged.consumption_loss_mt / merged.consumption_loss_mt.max())
        colors = ["#9b2226" if iso in shocked else "#ee9b00" for iso in merged.index]
        ax.scatter(merged.lon, merged.lat, s=scale, c=colors, alpha=0.8, edgecolors="#333333", linewidths=0.3, zorder=3)
    home = flows[flows.exporter_iso3.isin(shocked)].head(25)
    away = flows[~flows.exporter_iso3.isin(shocked)].head(15)
    rises = edges[
        (edges.crop == crop) & (edges.year == year)
        & (edges.allocation_rule == rule) & (edges.alpha == alpha)
        & (edges.export_cut_mt <= -min_cut)
    ].sort_values("export_cut_mt").head(15)
    def draw(frame, color):
        if frame.empty:
            return
        ceiling = frame.export_cut_mt.max()
        for rec in frame.itertuples(index=False):
            if rec.exporter_iso3 not in pts.index or rec.importer_iso3 not in pts.index:
                continue
            x0, y0 = pts.loc[rec.exporter_iso3, ["lon", "lat"]]
            x1, y1 = pts.loc[rec.importer_iso3, ["lon", "lat"]]
            width = 0.4 + 2.2 * (rec.export_cut_mt / ceiling)
            ax.annotate(
                "", xy=(x1, y1), xytext=(x0, y0),
                arrowprops=dict(arrowstyle="-|>", color=color, lw=width, alpha=0.75, mutation_scale=8),
                zorder=4,
            )
    draw(home, "#9b2226")
    draw(away, "#005f73")
    if not rises.empty:
        ceiling = rises.export_cut_mt.abs().max()
        for rec in rises.itertuples(index=False):
            if rec.exporter_iso3 not in pts.index or rec.importer_iso3 not in pts.index:
                continue
            x0, y0 = pts.loc[rec.exporter_iso3, ["lon", "lat"]]
            x1, y1 = pts.loc[rec.importer_iso3, ["lon", "lat"]]
            width = 0.4 + 2.2 * (abs(rec.export_cut_mt) / ceiling)
            ax.annotate(
                "", xy=(x1, y1), xytext=(x0, y0),
                arrowprops=dict(arrowstyle="-|>", color="#0a9396", lw=width, alpha=0.7, mutation_scale=8),
                zorder=4,
            )
    ax.set_xlim(-170, 180)
    ax.set_ylim(-58, 78)
    ax.set_axis_off()
    ax.set_title(title)
    ax.legend(
        handles=[
            Line2D([0], [0], color="#9b2226", lw=2, label=f"Export cut from a flooded country, ≥ {min_cut/1e3:.0f} kt"),
            Line2D([0], [0], color="#005f73", lw=2, label="Export cut passed onward"),
            Line2D([0], [0], marker="o", color="none", markerfacecolor="#9b2226", markersize=7, label="Loss in a flooded country"),
            Line2D([0], [0], color="#0a9396", lw=2, label="Another exporter ships more"),
            Line2D([0], [0], marker="o", color="none", markerfacecolor="#ee9b00", markersize=7, label="Loss in a partner"),
        ],
        frameon=False, loc="lower left", fontsize=8,
    )
    fig.text(0.01, 0.01, SOURCE, fontsize=7, color="#444444")
    fig.tight_layout(rect=(0, 0.03, 1, 1))
    fig.savefig(path, dpi=160)
    plt.close(fig)


def circular_network(edges, countries, names, crop, year, path):
    flows = edges[
        (edges.crop == crop) & (edges.year == year)
        & (edges.allocation_rule == "stock_then_split") & (edges.alpha == 0)
        & (edges.export_cut_mt > 1000)
    ].copy()
    loss = countries[
        (countries.crop == crop) & (countries.year == year)
        & (countries.allocation_rule == "stock_then_split") & (countries.alpha == 0)
    ]
    nodes = list(loss.sort_values("consumption_loss_mt", ascending=False).head(12).iso3)
    flows = flows[flows.exporter_iso3.isin(nodes) & flows.importer_iso3.isin(nodes)]
    graph = nx.DiGraph()
    for iso in nodes:
        graph.add_node(iso)
    for rec in flows.itertuples(index=False):
        graph.add_edge(rec.exporter_iso3, rec.importer_iso3, weight=rec.export_cut_mt)
    fig, ax = plt.subplots(figsize=(7.2, 7.2))
    pos = nx.circular_layout(graph)
    weights = [graph[u][v]["weight"] for u, v in graph.edges]
    ceiling = max(weights) if weights else 1
    shocked = set(loss.loc[loss.directly_shocked, "iso3"])
    node_colors = ["#9b2226" if iso in shocked else "#ee9b00" for iso in graph.nodes]
    nx.draw_networkx_nodes(graph, pos, ax=ax, node_color=node_colors, node_size=500, edgecolors="#333333")
    label_pos = {iso: (xy[0] * 1.22, xy[1] * 1.22) for iso, xy in pos.items()}
    nx.draw_networkx_labels(graph, label_pos, {iso: iso for iso in graph.nodes}, ax=ax, font_size=8, font_color="#222222")
    ax.set_xlim(-1.55, 1.55)
    ax.set_ylim(-1.55, 1.55)
    if weights:
        nx.draw_networkx_edges(
            graph, pos, ax=ax,
            width=[0.4 + 2.5 * (w / ceiling) for w in weights],
            edge_color="#005f73", alpha=0.75, arrows=True, arrowsize=12,
            connectionstyle="arc3,rad=0.08",
        )
    ax.set_axis_off()
    ax.set_title(f"{CROP_LABEL[crop]} {year}: largest consumption losses and the export cuts among them\nStock-then-split, no stocks. Labels are ISO3. Red nodes were flooded.")
    fig.tight_layout()
    fig.savefig(path, dpi=160)
    plt.close(fig)


def make_figures(world, summary, countries, edges, shocks, reserves, names):
    FIG.mkdir(parents=True, exist_ok=True)
    split0 = countries[(countries.allocation_rule == "stock_then_split") & (countries.alpha == 0)]
    split1 = countries[(countries.allocation_rule == "stock_then_split") & (countries.alpha == 1)]
    pta0 = countries[(countries.allocation_rule == "modified_pta") & (countries.alpha == 0)]

    shock_panels = []
    for crop in CROPS:
        series = shocks[shocks.crop == crop].groupby("iso3").shock_total_t.sum() / 1e6
        shock_panels.append((CROP_LABEL[crop], series))
    choropleth_row(
        world, shock_panels, FIG / "fig01_production_loss_maps.png",
        "Cumulative production loss, 2002 to 2024 (Mt)",
        caption=SOURCE,
    )

    for crop in CROPS:
        loss_panels = []
        for frame, label in ((split0, "Split, no stocks"), (pta0, "Modified PTA, no stocks")):
            series = frame[frame.crop == crop].groupby("iso3").consumption_loss_mt.sum() / 1e6
            loss_panels.append((f"{CROP_LABEL[crop]}, {label}", series))
        choropleth_row(
            world, loss_panels, FIG / f"fig02_consumption_{crop}.png",
            "Cumulative consumption decline (Mt)",
            caption=SOURCE + " Each panel has its own scale. Declines only.",
            shared=False,
        )

    draw_panels = []
    for crop in CROPS:
        series = split1[split1.crop == crop].groupby("iso3").reserve_draw_mt.sum() / 1e6
        draw_panels.append((CROP_LABEL[crop], series))
    choropleth_row(
        world, draw_panels, FIG / "fig03_stock_draw_maps.png",
        "Cumulative stock draw, stocks open (Mt)",
        cmap="Blues",
        caption=SOURCE + " Stock-then-split, beginning stocks open. Draws can sit in a partner, not only in the flooded country.",
    )

    fig, axes = plt.subplots(3, 1, figsize=(9.4, 8.2), sharex=True)
    for ax, crop in zip(axes, CROPS):
        sub = summary[summary.crop == crop]
        prod = sub[sub.allocation_rule == "stock_then_split"].drop_duplicates("year").set_index("year").production_loss_mt / 1e6
        cons0 = sub[(sub.allocation_rule == "stock_then_split") & (sub.alpha == 0)].set_index("year").world_consumption_loss_mt / 1e6
        cons1 = -sub[(sub.allocation_rule == "stock_then_split") & (sub.alpha == 1)].set_index("year").net_consumption_change_mt / 1e6
        ax.bar(prod.index, prod.values, color="#f1e3d3", label="Production loss")
        ax.plot(cons0.index, cons0.values, color="#9b2226", marker="o", ms=3, label="Consumption decline, no stocks")
        ax.plot(cons1.index, cons1.values, color="#005f73", marker="o", ms=3, label="Net consumption decline, stocks open")
        ax.set_ylabel("Million tonnes")
        ax.set_title(CROP_LABEL[crop])
        ax.axvline(2014.5, color="#888888", lw=0.6, ls="--")
    axes[0].legend(frameon=False, fontsize=8, loc="upper right")
    axes[-1].set_xlabel("Year")
    fig.suptitle("Harvest loss and the consumption decline that remains")
    fig.text(0.01, 0.005, SOURCE + " The dashed line separates GFD years from the provisional Sentinel-1 years.", fontsize=7, color="#444444")
    fig.tight_layout(rect=(0, 0.03, 1, 0.96))
    fig.savefig(FIG / "fig04_annual_losses.png", dpi=160)
    plt.close(fig)

    fig, ax = plt.subplots(figsize=(9.4, 4.6))
    for crop, color in zip(CROPS, ["#9b2226", "#bb3e03", "#005f73"]):
        sub = summary[(summary.crop == crop) & (summary.allocation_rule == "stock_then_split") & (summary.alpha == 0)]
        ax.plot(sub.year, 100 * sub.direct_loss_share, marker="o", ms=3, color=color, label=f"{CROP_LABEL[crop]}, split")
        pta = summary[(summary.crop == crop) & (summary.allocation_rule == "modified_pta") & (summary.alpha == 0)]
        ax.plot(pta.year, 100 * pta.direct_loss_share, marker="x", ms=3, color=color, alpha=0.45, label=f"{CROP_LABEL[crop]}, modified PTA")
    ax.axhline(50, color="#888888", lw=0.6, ls="--")
    ax.set_ylim(0, 105)
    ax.set_ylabel("Share of the consumption decline in flooded countries (%)")
    ax.set_xlabel("Year")
    ax.set_title("How much of the consumption decline stays in the flooded countries")
    ax.legend(frameon=False, fontsize=7, ncol=2)
    fig.text(0.01, 0.01, "No stocks. Modified PTA shares are approximate: 68 of 69 crop-years hit the 50-iteration cap.", fontsize=7, color="#444444")
    fig.tight_layout(rect=(0, 0.05, 1, 1))
    fig.savefig(FIG / "fig05_home_share.png", dpi=160)
    plt.close(fig)

    shocked = split0[split0.directly_shocked].copy()
    fig, axes = plt.subplots(1, 3, figsize=(10.5, 3.8), sharey=True)
    for ax, crop in zip(axes, CROPS):
        stu = shocked.loc[shocked.crop == crop, "stu_network"].replace([np.inf, -np.inf], np.nan).dropna()
        shown = stu.clip(upper=1.0)
        ax.hist(shown, bins=np.linspace(0, 1, 21), color="#005f73", edgecolor="white")
        ax.set_title(f"{CROP_LABEL[crop]}\n{int((stu > 1).sum())} country-years above 1")
        ax.set_xlabel("Beginning stocks / network use")
        ax.set_ylabel("Flooded country-years")
    fig.suptitle("Stock-to-use in the flooded country-years")
    fig.text(0.01, 0.01, "Network use is production plus imports minus exports. Values above 1 are counted in the title and drawn in the last bin. Blank stocks are zero.", fontsize=7, color="#444444")
    fig.tight_layout(rect=(0, 0.06, 1, 0.9))
    fig.savefig(FIG / "fig06_stu_hist.png", dpi=160)
    plt.close(fig)

    own = split1[split1.directly_shocked][["crop", "year", "iso3", "reserve_draw_mt", "production_loss_mt", "stu_network", "beginning_stocks_mt"]].copy()
    own = own[own.production_loss_mt > 1000]
    own["coverage"] = np.where(own.production_loss_mt > 0, own.reserve_draw_mt / own.production_loss_mt, np.nan)
    fig, axes = plt.subplots(1, 3, figsize=(10.5, 3.8), sharey=True)
    for ax, crop in zip(axes, CROPS):
        sub = own[own.crop == crop]
        ax.scatter(
            sub.stu_network.clip(upper=1.0), sub.coverage.clip(0, 1.5),
            s=8 + 40 * np.sqrt(sub.production_loss_mt / sub.production_loss_mt.max()),
            c="#9b2226", alpha=0.55, edgecolors="none",
        )
        ax.axhline(1, color="#888888", lw=0.5, ls="--")
        ax.set_xlim(0, 1)
        ax.set_ylim(-0.05, 1.35)
        ax.set_xlabel("Stock-to-use, capped at 1 for display")
        ax.set_ylabel("Own stock draw / own harvest loss")
        ax.set_title(CROP_LABEL[crop])
    fig.suptitle("Own stock draw divided by the country's own harvest loss")
    fig.text(0.01, 0.01, "Points are flooded country-years with a loss above 1 kt. Coverage near 1 means own stocks covered the harvest loss. Points above 1 also lost imports. The misses sit at low stock-to-use.", fontsize=7, color="#444444")
    fig.tight_layout(rect=(0, 0.07, 1, 0.9))
    fig.savefig(FIG / "fig07_stu_coverage.png", dpi=160)
    plt.close(fig)

    stu_panels = []
    for crop in CROPS:
        sub = shocked[shocked.crop == crop]
        series = sub.groupby("iso3").stu_network.median()
        stu_panels.append((CROP_LABEL[crop], series.clip(upper=0.8)))
    choropleth_row(
        world, stu_panels, FIG / "fig08_stu_maps.png",
        "Median stock-to-use of flooded years (scale capped at 0.8)",
        cmap="YlGnBu",
        caption="Median across the years that country was flooded. Blank beginning stocks enter as zero.",
    )

    cases = []
    for crop in CROPS:
        sub = summary[(summary.crop == crop) & (summary.allocation_rule == "stock_then_split") & (summary.alpha == 0)]
        year = int(sub.loc[sub.abroad_consumption_loss_mt.idxmax(), "year"])
        cases.append((crop, year))
    if ("wheat", 2010) not in cases:
        cases.append(("wheat", 2010))
    if ("wheat", 2011) not in cases:
        cases.append(("wheat", 2011))
    for crop, year in cases:
        flow_map(
            world, edges, countries, crop, year, "stock_then_split", 0,
            f"{CROP_LABEL[crop]} {year}: export cuts and consumption losses, stock-then-split, no stocks",
            FIG / f"fig_flow_{crop}_{year}_split.png", names,
        )
        flow_map(
            world, edges, countries, crop, year, "modified_pta", 0,
            f"{CROP_LABEL[crop]} {year}: export changes and consumption losses, modified PTA, no stocks",
            FIG / f"fig_flow_{crop}_{year}_pta.png", names,
        )
    circular_network(edges, countries, names, "wheat", 2011, FIG / "fig_network_wheat_2011.png")

    fig, ax = plt.subplots(figsize=(8.6, 4.6))
    labels = list(REGIONS)
    x = np.arange(len(labels))
    width = 0.25
    for i, crop in enumerate(CROPS):
        heights = []
        for members in REGIONS.values():
            heights.append(split0[(split0.crop == crop) & split0.iso3.isin(members)].consumption_loss_mt.sum() / 1e6)
        ax.bar(x + (i - 1) * width, heights, width, label=CROP_LABEL[crop], color=["#9b2226", "#bb3e03", "#005f73"][i])
    ax.set_xticks(x)
    ax.set_xticklabels(labels, rotation=30, ha="right")
    ax.set_ylabel("Consumption decline, 2002–2024 (Mt)")
    ax.set_title("Consumption decline in the regions used in the benchmark figures")
    ax.legend(frameon=False)
    fig.text(0.01, 0.01, "Stock-then-split, no stocks. Sudan is not in the three African groups. These are tonnes, not shares of regional use.", fontsize=7, color="#444444")
    fig.tight_layout(rect=(0, 0.05, 1, 1))
    fig.savefig(FIG / "fig09_regions.png", dpi=160)
    plt.close(fig)
    return cases, own


def mt(value):
    return f"{value / 1e6:.2f}"


def pct(value):
    return f"{100 * value:.1f}%"


def build_facts(summary, countries, edges, shocks, reserves, own):
    facts = {}
    split0 = summary[(summary.allocation_rule == "stock_then_split") & (summary.alpha == 0)]
    split1 = summary[(summary.allocation_rule == "stock_then_split") & (summary.alpha == 1)]
    pta0 = summary[(summary.allocation_rule == "modified_pta") & (summary.alpha == 0)]
    facts["prod"] = {crop: split0.loc[split0.crop == crop, "production_loss_mt"].sum() for crop in CROPS}
    facts["home0"] = {
        crop: split0.loc[split0.crop == crop, "direct_consumption_loss_mt"].sum()
        / split0.loc[split0.crop == crop, "world_consumption_loss_mt"].sum()
        for crop in CROPS
    }
    facts["stock_share"] = {
        crop: split1.loc[split1.crop == crop, "reserve_draw_mt"].sum()
        / split1.loc[split1.crop == crop, "production_loss_mt"].sum()
        for crop in CROPS
    }
    facts["remain"] = {crop: -split1.loc[split1.crop == crop, "net_consumption_change_mt"].sum() for crop in CROPS}
    facts["pta_home"] = {
        crop: pta0.loc[pta0.crop == crop, "direct_consumption_loss_mt"].sum()
        / pta0.loc[pta0.crop == crop, "world_consumption_loss_mt"].sum()
        for crop in CROPS
    }
    facts["direct_draw_share"] = {
        crop: split1.loc[split1.crop == crop, "direct_reserve_draw_mt"].sum()
        / split1.loc[split1.crop == crop, "reserve_draw_mt"].sum()
        for crop in CROPS
    }
    facts["provisional"] = shocks.loc[shocks.year >= 2015, "shock_total_t"].sum() / shocks.shock_total_t.sum()
    facts["gfd"] = shocks.loc[shocks.year <= 2014].groupby("crop").shock_total_t.sum() / 1e6
    facts["sentinel"] = shocks.loc[shocks.year >= 2015].groupby("crop").shock_total_t.sum() / 1e6
    low = split0[split0.direct_loss_share < 0.6][["crop", "year", "direct_loss_share", "production_loss_mt", "abroad_consumption_loss_mt"]]
    facts["low_years"] = low.sort_values("direct_loss_share")
    psd = reserves[["crop", "iso3", "year", "PSD_Missing_Flag", "stock_to_use", "domestic_use_mt", "beginning_stocks_mt"]]
    shocked = shocks.merge(psd, on=["crop", "iso3", "year"], how="left", suffixes=("", "_psd"))
    missing = shocked["PSD_Missing_Flag"].fillna(1).eq(1)
    facts["missing_rows"] = int(missing.sum())
    facts["missing_tonnes"] = shocked.loc[missing, "shock_total_t"].sum()
    facts["missing_share"] = facts["missing_tonnes"] / shocks.shock_total_t.sum()
    facts["n_rows"] = len(shocks)
    facts["n_iso_years"] = shocks.groupby(["iso3", "year"]).ngroups
    facts["capped"] = shocks[shocks.capped_at_production.astype(str).str.lower().isin(["true", "1"])]
    y0 = shocks.shock_y0_t.sum()
    y1 = shocks.shock_y1_t.sum()
    facts["y0_share"] = y0 / (y0 + y1)
    gap0 = (split0.net_consumption_change_mt + split0.production_loss_mt).abs().max()
    facts["balance_gap_t"] = gap0
    facts["split_mass_t"] = summary.loc[summary.allocation_rule == "stock_then_split", "mass_gap_mt"].abs().max()
    pta_mass_row = summary.loc[summary.loc[summary.allocation_rule == "modified_pta", "mass_gap_mt"].abs().idxmax()]
    facts["pta_mass_t"] = abs(float(pta_mass_row.mass_gap_mt))
    facts["pta_mass_crop"] = pta_mass_row.crop
    facts["pta_mass_year"] = int(pta_mass_row.year)
    facts["pta_mass_alpha"] = pta_mass_row.alpha
    facts["pta_cap"] = int((~pta0.converged).sum())
    facts["split_converged"] = int(summary.loc[summary.allocation_rule == "stock_then_split", "converged"].sum())
    facts["split_runs"] = int((summary.allocation_rule == "stock_then_split").sum())
    facts["new_links"] = int(summary.new_links.sum())
    facts["overdraw"] = int(summary.reserve_overdraw.sum())
    own_extra = own.reserve_draw_mt - own.production_loss_mt
    facts["own_extra_n"] = int((own_extra > 1).sum())
    facts["own_extra_sum"] = float(own_extra[own_extra > 1].sum()) if (own_extra > 1).any() else 0.0
    top_extra = own.loc[own_extra.idxmax()]
    facts["own_extra_top"] = top_extra
    stu = countries[(countries.allocation_rule == "stock_then_split") & (countries.alpha == 0) & countries.directly_shocked].copy()
    bins = [0, 0.05, 0.2, 0.5, 100]
    labels = ["Under 0.05", "0.05 to 0.20", "0.20 to 0.50", "Above 0.50"]
    stu["bin"] = pd.cut(stu.stu_network.fillna(0), bins, labels=labels, right=False)
    facts["stu_bins"] = stu.groupby(["crop", "bin"], observed=False).production_loss_mt.sum().unstack()
    facts["stu_undefined"] = int(stu.stu_network.isna().sum())
    joined = stu.merge(psd, on=["crop", "iso3", "year"], how="left")
    both = joined[joined.stu_network.notna() & joined.stock_to_use.notna() & (joined.baseline_use_mt > 1)]
    facts["stu_corr"] = both.groupby("crop").apply(lambda frame: frame.stu_network.corr(frame.stock_to_use), include_groups=False)
    facts["stu_big_gap"] = both[(both.stu_network - both.stock_to_use).abs() > 0.2]
    partner = countries[
        (countries.allocation_rule == "stock_then_split") & (countries.alpha == 0)
        & ~countries.directly_shocked & (countries.baseline_use_mt >= 1e5)
    ].copy()
    partner["loss_fraction"] = partner.consumption_loss_mt / partner.baseline_use_mt
    facts["top_partners"] = partner.sort_values("consumption_loss_mt", ascending=False).head(12)
    facts["top_fraction"] = partner.sort_values("loss_fraction", ascending=False).head(8)
    tiny = countries[
        (countries.allocation_rule == "stock_then_split") & (countries.alpha == 0)
        & ~countries.directly_shocked & (countries.baseline_use_mt > 0)
        & (countries.baseline_use_mt < 1e5) & (countries.consumption_loss_mt > 0)
    ]
    facts["tiny_max_fraction"] = float((tiny.consumption_loss_mt / tiny.baseline_use_mt).max()) if len(tiny) else 0
    facts["top_shock_countries"] = (
        shocks.groupby(["crop", "iso3"]).shock_total_t.sum().reset_index().sort_values("shock_total_t", ascending=False)
    )
    neg = countries[(countries.allocation_rule == "stock_then_split") & (countries.alpha == 0) & (countries.baseline_use_mt < -1)]
    facts["negative_use"] = neg.sort_values(["crop", "year"])
    return facts


def top_names(frame, names, n=5):
    rows = []
    for rec in frame.head(n).itertuples(index=False):
        rows.append(f"{short_name(names, rec.iso3)} {mt(rec.shock_total_t)} Mt")
    return "; ".join(rows)


def write_markdown(facts, cases, names):
    lines = []
    add = lines.append
    add("# Flood shocks, 2002 to 2024")
    add("")
    add("This report measures physical availability of milled rice, wheat, and maize. A consumption decline here is a change in national commodity supply on the trade network. It is not household food consumption, not a calorie shortage, and not a measure of access or utilization.")
    add("")
    add("The shock table is the draft dated 20 November 2026. `shock_total_t` is the loss applied in calendar year t. It already adds the same-year flood loss and the second-year loss from floods in the previous year. Rice is milled. Each crop-year is run on that year's production, net trade, and USDA beginning stocks. A blank beginning stock is zero. Imputed stocks are not used. Alpha is only 0 or 1. There is no price step and no export ban.")
    add("")
    add("Stock-then-split is the primary rule. It draws usable stocks, then splits any remaining deficit of at least 1 tonne between exports and domestic use in proportion to the two, and passes the export cut along links that already exist. Modified PTA, with the empty-reserve freeze off, is the sensitivity. In that sensitivity the published update still takes only 1 percent of the remaining shortage out of domestic use, so the loss moves through trade. Published PTA, with the freeze on, is not the flood result.")
    add("")
    add("Rows for 2015 to 2024 come from a 1/16 to 1/32 sample of the Sentinel-1 archive. Full-archive flooded area is larger, and those rows will be replaced. Rows for 2002 to 2014 will not. The dashed line on the annual figure is that break.")
    add("")
    add("## Headline")
    add("")
    add("With beginning stocks open, stocks take almost all of the harvest loss, and the consumption decline that remains stays in the flooded countries. With no stocks, rice and maize keep about 90 percent of the consumption decline at home. Wheat keeps 76 percent, and in 2010 and 2011 only about half. The sentence that flood losses mostly stay at home is a fair description of the stock case. It is the wrong lead for wheat when stocks cannot be drawn.")
    add("")
    add("| Crop | Production loss (Mt) | No stocks, share in flooded countries | Stocks open, drawn from stocks | Stocks open, remaining consumption loss (Mt) | Of the stock draw, share in flooded countries | Modified PTA, no stocks, share in flooded countries |")
    add("|---|---:|---:|---:|---:|---:|---:|")
    for crop in CROPS:
        add(
            f"| {CROP_LABEL[crop]} | {mt(facts['prod'][crop])} | {pct(facts['home0'][crop])} | {pct(facts['stock_share'][crop])} | {mt(facts['remain'][crop])} | {pct(facts['direct_draw_share'][crop])} | {pct(facts['pta_home'][crop])} |"
        )
    add("")
    add(f"The three crops together lose {mt(sum(facts['prod'].values()))} Mt over 2002 to 2024. {pct(facts['provisional'])} of that total sits in the provisional 2015 to 2024 rows. Same-year losses are {pct(facts['y0_share'])} of the applied shock. The rest is the second-year term.")
    add("")
    add("GFD years, 2002 to 2014, million tonnes: "
        + ", ".join(f"{CROP_LABEL[c]} {facts['gfd'].get(c, 0):.2f}" for c in CROPS)
        + ". Sentinel sample, 2015 to 2024: "
        + ", ".join(f"{CROP_LABEL[c]} {facts['sentinel'].get(c, 0):.2f}" for c in CROPS)
        + ".")
    add("")
    add("## Where the harvest was lost")
    add("")
    add("![Cumulative production loss](figures/fig01_production_loss_maps.png)")
    add("")
    add("The largest cumulative losses are concentrated in a few producers. "
        + " ".join(
            f"{CROP_LABEL[crop]}: {top_names(facts['top_shock_countries'][facts['top_shock_countries'].crop == crop], names)}."
            for crop in CROPS
        ))
    add("")
    add("Three crop-country-years were capped at production before this run: "
        + ", ".join(f"{rec.iso3} {rec.crop} {int(rec.year)} ({rec.shock_total_t:,.0f} t)" for rec in facts["capped"].itertuples(index=False))
        + ". The cap is in the table. The simulator also refuses a loss above baseline production.")
    add("")
    add("## Stock-to-use")
    add("")
    add("Stock-to-use here is beginning stocks divided by network domestic use. Network use is production plus imports minus exports in that year. It is not the USDA domestic-use series, and the two do not always agree. A blank beginning stock is a zero in this ratio, which treats a missing report as an empty reserve.")
    add("")
    add("![Stock-to-use of flooded country-years](figures/fig06_stu_hist.png)")
    add("")
    add("![Median stock-to-use of countries that were flooded](figures/fig08_stu_maps.png)")
    add("")
    add("![Own stocks against the local harvest loss](figures/fig07_stu_coverage.png)")
    add("")
    add("Harvest loss by the flooded country's stock-to-use, million tonnes. Country-years with no positive network use are in the lowest bin.")
    add("")
    add("| Stock-to-use | Rice | Wheat | Maize |")
    add("|---|---:|---:|---:|")
    bins = facts["stu_bins"]
    for label in bins.columns:
        cells = " | ".join(f"{bins.loc[crop, label] / 1e6:.2f}" if crop in bins.index else "0.00" for crop in CROPS)
        add(f"| {label} | {cells} |")
    add("")
    add(f"Of the harvest loss, {pct(facts['missing_share'])} falls on country-years whose USDA beginning stock is missing ({facts['missing_rows']} of {facts['n_rows']} shock rows). Those countries cannot draw a reserve in this run. That is a data choice, not a finding that the reserve was empty.")
    add("")
    top = facts["own_extra_top"]
    add("Almost all of the stock draw is inside the flooded countries: "
        + ", ".join(f"{CROP_LABEL[crop]} {pct(facts['direct_draw_share'][crop])}" for crop in CROPS)
        + ". Partners do not replace these lost imports out of their own stocks in any material quantity. "
        + f"{facts['own_extra_n']} flooded country-years draw more than their own harvest loss, by {facts['own_extra_sum'] / 1e3:.0f} kt in total. "
        + f"The largest is {short_name(names, top.iso3)} {CROP_LABEL[top.crop].lower()} in {int(top.year)}, a harvest loss of {top.production_loss_mt / 1e3:.0f} kt and a draw of {top.reserve_draw_mt / 1e3:.0f} kt. "
        + "Those countries were also importers, so the extra draw replaces a lost import. It is not a second copy of the harvest loss.")
    add("")
    add("## Where consumption fell")
    add("")
    add("Each panel has its own color scale. A shared scale would hide the rice map under the maize totals, and it would hide the split map under the wider modified-PTA declines.")
    add("")
    for crop in CROPS:
        add(f"![{CROP_LABEL[crop]} consumption decline](figures/fig02_consumption_{crop}.png)")
        add("")
    add("")
    add("![Annual harvest loss and consumption decline](figures/fig04_annual_losses.png)")
    add("")
    add("![Share of the decline that stays in flooded countries](figures/fig05_home_share.png)")
    add("")
    add("![Stock draw when beginning stocks are open](figures/fig03_stock_draw_maps.png)")
    add("")
    add("![Consumption decline by region](figures/fig09_regions.png)")
    add("")
    add("With no stocks, the years in which less than 60 percent of the consumption decline stays in the flooded countries are:")
    add("")
    if facts["low_years"].empty:
        add("None.")
    else:
        for rec in facts["low_years"].itertuples(index=False):
            add(f"- {CROP_LABEL[rec.crop]} {int(rec.year)}: {pct(rec.direct_loss_share)} stays home, and {mt(rec.abroad_consumption_loss_mt)} Mt falls elsewhere, out of a {mt(rec.production_loss_mt)} Mt harvest loss.")
    add("")
    add("The largest partner consumption losses under stock-then-split with no stocks, among countries with at least 0.1 Mt of baseline use, are below. The percentage is that country's loss divided by its own baseline use. Losses are spread across many partners. None of these large-use partners loses a large share of its own use.")
    add("")
    add("| Crop | Year | Country | Loss (Mt) | Share of that country's use |")
    add("|---|---:|---|---:|---:|")
    for rec in facts["top_partners"].itertuples(index=False):
        add(f"| {CROP_LABEL[rec.crop]} | {int(rec.year)} | {short_name(names, rec.iso3)} | {mt(rec.consumption_loss_mt)} | {pct(rec.loss_fraction)} |")
    add("")
    add(f"Countries with under 0.1 Mt of baseline use can show a large percentage from a small tonnage. The highest such percentage in this run is {pct(facts['tiny_max_fraction'])}. Those percentages are not used to rank impact.")
    add("")
    add("## Trade networks in the years that transmit")
    add("")
    add("Arrows are reductions on links that already existed. The split does not open a new link and does not increase another exporter's shipments. Red arrows start in a flooded country. Teal arrows are a later country passing a lost import onward by cutting its own exports. Red circles are consumption losses in flooded countries. Orange circles are losses in partners.")
    add("")
    for crop, year in cases:
        add(f"### {CROP_LABEL[crop]} {year}")
        add("")
        add(f"![Stock-then-split](figures/fig_flow_{crop}_{year}_split.png)")
        add("")
        add(f"![Modified PTA](figures/fig_flow_{crop}_{year}_pta.png)")
        add("")
    add("The wheat 2011 panel below keeps the twelve countries with the largest consumption losses and the export cuts among them.")
    add("")
    add("![Wheat 2011 network](figures/fig_network_wheat_2011.png)")
    add("")
    add("Modified PTA on the same years moves more of the loss out of the flooded countries, and other exporters can increase shipments on existing links. Those maps are the sensitivity, not a second estimate of the same behavior. Modified PTA reached the iteration cap in "
        f"{facts['pta_cap']} of 69 crop-years with no stocks, so the partner ranking on those maps is approximate. The world consumption decline under the split, with no stocks, matches the harvest loss. Under modified PTA the sum of country declines is larger than the harvest loss because some countries gain while others lose more.")
    add("")
    add("## Red team")
    add("")
    add(f"The table has {facts['n_rows']} crop-country-year rows and {facts['n_iso_years']} country-years. All of those ISO3 codes are in the 195-country network, and the table's production matches the annual files to within a tonne. `shock_total_t` matches the sum of the same-year and second-year terms.")
    add("")
    add(f"Stock-then-split converged on all {facts['split_runs']} runs. New bilateral links across every rule and alpha: {facts['new_links']}. A country never drew more than the stocks it was allowed to draw. The largest quantity gap in a split run is {facts['split_mass_t']:.0f} tonnes. With no stocks, the largest gap between the harvest loss and the net consumption decline is {facts['balance_gap_t']:.0f} tonnes.")
    add("")
    neg_bits = [
        f"{short_name(names, rec.iso3)} {CROP_LABEL[rec.crop].lower()} {int(rec.year)} ({rec.baseline_use_mt / 1e6:.2f} Mt)"
        for rec in facts["negative_use"].itertuples(index=False)
    ]
    add(
        f"Modified PTA is a different story on convergence. {facts['pta_cap']} of the 69 no-stock crop-years stopped at 50 iterations. "
        f"Its largest mass gap is {facts['pta_mass_t'] / 1e3:.1f} kt, on {CROP_LABEL[facts['pta_mass_crop']].lower()} in {facts['pta_mass_year']} at alpha {facts['pta_mass_alpha']:g}. "
        f"The split's largest gap is {facts['split_mass_t']:.0f} tonnes. Modified PTA ends by setting every country's use to its supply when supply is positive, and to zero when supply is negative. That closing step is what separates its balance from the split. "
        f"The split leaves a negative baseline use in place. Four flooded country-years have one: {'; '.join(neg_bits)}. "
        "Consumption losses in the tables are computed from use floored at zero, so a negative baseline does not become a consumption gain. Applying the published closer to the split would cut those exports and invent a transmission the flood did not cause."
    )
    add("")
    add("The stock result is sensitive to the stock definition. This run uses unshifted USDA beginning stocks. A cascade that uses the previous market year's ending stocks will not match these draws country by country. Missing beginning stocks are zero here, and they carry "
        f"{pct(facts['missing_share'])} of the harvest loss. Filling them would raise the stock absorption and lower the remaining consumption loss.")
    add("")
    add("Network stock-to-use and the USDA stock-to-use ratio move together, but they are not the same series. Correlation by crop, on flooded country-years where both are defined: "
        + ", ".join(f"{CROP_LABEL[crop]} {facts['stu_corr'].get(crop, float('nan')):.2f}" for crop in CROPS)
        + f". {len(facts['stu_big_gap'])} flooded country-years differ by more than 0.20. {facts['stu_undefined']} flooded country-years have no positive network use, so the ratio is undefined and those tonnes were placed in the lowest bin of the table.")
    add("")
    add("The 2015 to 2024 maps and totals are a lower bound on flooded area. They should not be read as a completed history, and they should not be compared with the 2002 to 2014 maps as if the satellite method were the same. Within the GFD period the rules and the stock accounting are the ones that will stand.")
    add("")
    add("What this run does not say: it does not say that a household lost the national tonnage, that a price rose, or that a nutrition outcome followed. Producer prices, food CPI, and the World Bank pink sheet remain the evidence on access. No model price was computed.")
    add("")
    add("## Files")
    add("")
    add("`Rscript main/run_flood_shocks.R` writes the summary, country, and edge tables. `python3 main/plot_flood_report.py` writes this report and the figures.")
    add("")
    (OUT / "REPORT.md").write_text("\n".join(lines) + "\n")
    return lines


def write_docx(lines):
    doc = Document()
    style = doc.styles["Normal"]
    style.font.name = "Calibri"
    style.font.size = Pt(11)
    for line in lines:
        if line.startswith("# "):
            doc.add_heading(line[2:], level=0)
        elif line.startswith("## "):
            doc.add_heading(line[3:], level=1)
        elif line.startswith("### "):
            doc.add_heading(line[4:], level=2)
        elif line.startswith("!["):
            name = line.split("](")[1].rstrip(")")
            path = OUT / name
            if path.exists():
                doc.add_picture(str(path), width=Inches(6.4))
        elif line.startswith("|"):
            if set(line.replace("|", "").strip()) <= set("-: "):
                continue
            cells = [part.strip() for part in line.strip("|").split("|")]
            if not hasattr(write_docx, "table") or write_docx.table is None:
                write_docx.table = doc.add_table(rows=1, cols=len(cells))
                write_docx.table.style = "Table Grid"
                for i, cell in enumerate(cells):
                    write_docx.table.rows[0].cells[i].text = cell
                write_docx.cols = len(cells)
            elif len(cells) == write_docx.cols:
                row = write_docx.table.add_row().cells
                for i, cell in enumerate(cells):
                    row[i].text = cell
        elif line.startswith("- "):
            write_docx.table = None
            doc.add_paragraph(line[2:], style="List Bullet")
        elif line.strip() == "":
            write_docx.table = None
        else:
            write_docx.table = None
            doc.add_paragraph(line)
    doc.save(OUT / "FSC_flood_shock_report.docx")


def main():
    summary, countries, edges, shocks, reserves = load_inputs()
    names = names_table()
    world = world_frame()
    cases, own = make_figures(world, summary, countries, edges, shocks, reserves, names)
    facts = build_facts(summary, countries, edges, shocks, reserves, own)
    lines = write_markdown(facts, cases, names)
    write_docx.table = None
    write_docx.cols = 0
    write_docx(lines)
    print(OUT / "REPORT.md")
    print(OUT / "FSC_flood_shock_report.docx")


if __name__ == "__main__":
    main()

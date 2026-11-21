"""Figures 2 to 5 of Puma et al. (2015), redrawn from current data.

- Figure 2: exporters that make up 90% of world wheat and rice exports,
  2004-2006 mean, with an asterisk on countries listed in
  ``ancillary/export_restrictions_2008.csv``.
- Figure 3: the two continental disturbances. Left, 1816 June-August
  temperature anomaly relative to 1971-2000 from the Luterbacher et al.
  (2004) European seasonal reconstruction. Right, mean 1876-1878 June-August
  PDSI from the Monsoon Asia Drought Atlas (Cook et al. 2010).
- Figure 4: export strength versus export degree with a least-squares power
  law and the component-wise median, by crop and period.
- Figure 5: maps of self-sufficiency in two periods and the change between them.

Paleoclimate files and Natural Earth country outlines are downloaded on
first use into ``erl2015/data/`` (gitignored).
"""

from __future__ import annotations

import zipfile
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt  # noqa: E402
import numpy as np  # noqa: E402
import pandas as pd  # noqa: E402
from matplotlib.colors import BoundaryNorm, TwoSlopeNorm  # noqa: E402

from . import config, data, metrics  # noqa: E402
from .network import build_networks  # noqa: E402
from .report import INK, INK2, SERIES, SURFACE, _style  # noqa: E402

PALEO_DIR = config.DATA_DIR / "paleo"
LUTERBACHER_URL = "https://www.ncei.noaa.gov/pub/data/paleo/historical/europe-seasonal-files/{year}-su.txt"
MADA_URL = "https://www.ncei.noaa.gov/pub/data/paleo/treering/reconstructions/asia/cook2010pdsi/MADApdsi.nc"
NATURAL_EARTH_URL = "https://naciscdn.org/naturalearth/50m/cultural/ne_50m_admin_0_countries.zip"
CROP_LABEL = {"wheat": "Wheat", "rice": "Rice (milled)"}
NEUTRAL = "#e4e3df"
NO_DATA = "#c9c7c1"


def _download(url: str, dest: Path) -> Path:
    import requests

    if dest.exists() and dest.stat().st_size > 0:
        return dest
    dest.parent.mkdir(parents=True, exist_ok=True)
    resp = requests.get(url, timeout=120, headers={"User-Agent": "fsc2015-figures"})
    if resp.status_code >= 400:
        raise data.DataError(f"Download refused ({resp.status_code}): {url}")
    tmp = dest.with_suffix(dest.suffix + ".partial")
    tmp.write_bytes(resp.content)
    tmp.replace(dest)
    return dest


# --------------------------------------------------------------------------
# Inputs for maps


def countries_outline():
    """Natural Earth 1:50m countries as a GeoDataFrame with an ``m49`` column."""
    import geopandas as gpd

    zp = _download(NATURAL_EARTH_URL, config.DATA_DIR / "ne_50m_admin_0_countries.zip")
    shp = config.DATA_DIR / "ne_50m_admin_0_countries" / "ne_50m_admin_0_countries.shp"
    if not shp.exists():
        with zipfile.ZipFile(zp) as zf:
            zf.extractall(shp.parent)
    g = gpd.read_file(shp)
    code = g["ISO_N3_EH"].where(g["ISO_N3_EH"] != "-99", g["ISO_N3"])
    g["m49"] = code.astype(str).str.zfill(3)
    return g[["NAME", "m49", "geometry"]]


def luterbacher_jja(year: int) -> np.ndarray:
    """June-August temperature (deg C) on the 0.5 degree European grid, 70 x 130.

    Rows run north (69.75N) to south (35.25N); columns west (24.75W) to east (39.75E).
    """
    path = _download(LUTERBACHER_URL.format(year=year), PALEO_DIR / f"{year}-su.txt")
    v = np.array(path.read_text().split(), dtype=float)
    if v.size != 70 * 130:
        raise data.DataError(f"{path.name}: expected 9100 values, found {v.size}")
    v[v <= -999] = np.nan
    return v.reshape(70, 130)


LUT_LON = -24.75 + 0.5 * np.arange(130)
LUT_LAT = 69.75 - 0.5 * np.arange(70)


def summer_1816_anomaly(base=(1971, 2000)) -> np.ndarray:
    clim = np.nanmean([luterbacher_jja(y) for y in range(base[0], base[1] + 1)], axis=0)
    return luterbacher_jja(1816) - clim


def mada_pdsi(years=(1876, 1877, 1878)):
    """Mean JJA PDSI over ``years`` from the Monsoon Asia Drought Atlas."""
    from scipy.io import netcdf_file

    path = _download(MADA_URL, PALEO_DIR / "MADApdsi.nc")
    with netcdf_file(path, "r", mmap=False) as f:
        t = np.asarray(f.variables["time"][:]).astype(int)
        p = np.asarray(f.variables["PDSI"][:], dtype=float)
        lat = np.asarray(f.variables["lat"][:], dtype=float)
        lon = np.asarray(f.variables["lon"][:], dtype=float)
    p[np.abs(p) > 9.99] = np.nan  # the atlas stores no-data as +/-9.999
    idx = [int(np.flatnonzero(t == y)[0]) for y in years]
    return lon, lat, np.nanmean(p[idx], axis=0)


# --------------------------------------------------------------------------
# Figure 2


def load_restrictions(path: Path | None = None) -> pd.DataFrame:
    path = path or config.ANCILLARY_DIR / "export_restrictions_2008.csv"
    df = pd.read_csv(path, dtype={"m49": str}, comment="#")
    for col in ("crop", "m49", "name", "source"):
        if col not in df.columns:
            raise data.DataError(f"{path.name}: missing column {col}")
    df["m49"] = df["m49"].str.zfill(3)
    return df


def top_exporters(nets: dict, years, share: float = 0.9) -> pd.DataFrame:
    """Exporters whose mean exports over ``years`` together reach ``share`` of the total."""
    tabs = [metrics.node_table(nets[y]) for y in years]
    t = pd.concat(tabs).groupby(["fao", "name"], as_index=False)["s_out_t"].mean()
    t = t[t["s_out_t"] > 0].sort_values("s_out_t", ascending=False).reset_index(drop=True)
    t["share"] = t["s_out_t"] / t["s_out_t"].sum()
    t["cum_share"] = t["share"].cumsum()
    n = int(np.searchsorted(t["cum_share"].to_numpy(), share - 1e-9) + 1)
    return t.head(n)


def figure2(inp, path: Path, years=(2004, 2005, 2006), share: float = 0.9) -> Path:
    restr = load_restrictions()
    fig, axes = plt.subplots(2, 1, figsize=(9, 9), facecolor=SURFACE)
    for ax, crop in zip(axes, ("wheat", "rice")):
        nets = build_networks(inp.trade, crop, list(years), inp.countries)
        m49 = dict(zip(inp.countries["fao"], inp.countries["m49"].astype(str).str.zfill(3)))
        top = top_exporters(nets, years, share)
        flagged = set(restr.loc[restr["crop"] == crop, "m49"])
        labels = [f"{n}*" if m49.get(f) in flagged else n for f, n in zip(top["fao"], top["name"])]
        labels = [s.replace("United Kingdom of Great Britain and Northern Ireland", "United Kingdom")
                  .replace("United States of America", "USA").replace("Netherlands (Kingdom of the)", "Netherlands")
                  for s in labels]
        _style(ax)
        ax.bar(range(len(top)), top["s_out_t"] / 1e6, color=SERIES[0], width=0.7)
        ax.set_xticks(range(len(top)), labels, rotation=55, ha="right", fontsize=8.5)
        ax.set_ylabel(f"Exports, Mt/yr", color=INK2)
        ax.grid(axis="x", visible=False)
        ax.set_title(f"{CROP_LABEL[crop]}: {len(top)} exporters with {int(share * 100)}% of exports, "
                     f"{years[0]}-{years[-1]} mean", color=INK, fontsize=11, loc="left")
    note = "* restricted exports in 2007-2008 (ancillary/export_restrictions_2008.csv)" if len(restr) else \
        "No 2008 restriction list supplied; add rows to ancillary/export_restrictions_2008.csv for asterisks"
    fig.text(0.01, 0.005, note, color=INK2, fontsize=8)
    fig.tight_layout(rect=(0, 0.02, 1, 1))
    fig.savefig(path, dpi=200)
    plt.close(fig)
    return path


# --------------------------------------------------------------------------
# Figure 3


def figure3(path: Path) -> Path:
    world = countries_outline()
    anom = summer_1816_anomaly()
    lon, lat, pdsi = mada_pdsi()
    fig, axes = plt.subplots(1, 2, figsize=(12, 4.8), facecolor=SURFACE,
                             gridspec_kw={"width_ratios": [1.15, 1]})

    ax = axes[0]
    lim = np.nanmax(np.abs(anom))
    mesh = ax.pcolormesh(LUT_LON, LUT_LAT, anom, cmap="RdBu_r", norm=TwoSlopeNorm(0, -lim, lim), shading="nearest")
    world.boundary.plot(ax=ax, color=INK2, linewidth=0.4)
    ax.set_xlim(-25, 40); ax.set_ylim(35, 70)
    cb = fig.colorbar(mesh, ax=ax, shrink=0.85)
    cb.set_label("JJA 1816 minus 1971-2000, deg C", color=INK2)
    ax.set_title("Year Without a Summer, 1816", color=INK, fontsize=11, loc="left")

    ax = axes[1]
    levels = np.arange(-4, 4.5, 1.0)
    mesh = ax.pcolormesh(lon, lat, pdsi, cmap="BrBG", norm=BoundaryNorm(levels, 256, extend="both"),
                         shading="nearest")
    world.boundary.plot(ax=ax, color=INK2, linewidth=0.4)
    ax.set_xlim(lon.min() - 1.25, lon.max() + 1.25); ax.set_ylim(lat.min() - 1.25, lat.max() + 1.25)
    cb = fig.colorbar(mesh, ax=ax, shrink=0.85)
    cb.set_label("Mean JJA PDSI, 1876-1878 (negative = drought)", color=INK2)
    ax.set_title("Great Drought, 1876-1878", color=INK, fontsize=11, loc="left")
    for a in axes:
        a.set_facecolor(SURFACE)
        a.tick_params(colors=INK2, labelsize=8)
        a.set_aspect("equal")
    fig.text(0.01, 0.01, "Sources: Luterbacher et al. (2004) European seasonal temperature reconstruction; "
             "Cook et al. (2010) Monsoon Asia Drought Atlas. Both from NOAA NCEI Paleoclimatology.",
             color=INK2, fontsize=8)
    fig.tight_layout(rect=(0, 0.03, 1, 1))
    fig.savefig(path, dpi=200)
    plt.close(fig)
    return path


# --------------------------------------------------------------------------
# Figure 4


def figure4(inp, periods: dict, path: Path) -> tuple[Path, pd.DataFrame]:
    years = sorted({y for a, b in periods.values() for y in range(a, b + 1)})
    fig, axes = plt.subplots(2, len(periods), figsize=(4.2 * len(periods), 7.6), facecolor=SURFACE,
                             sharex=True, sharey="row", squeeze=False)
    rows = []
    for r, crop in enumerate(("wheat", "rice")):
        nets = build_networks(inp.trade, crop, years, inp.countries)
        for c, (label, (a, b)) in enumerate(periods.items()):
            ax = axes[r, c]
            _style(ax)
            nodes = metrics.period_nodes(nets, a, b)
            d = nodes[(nodes["k_out"] > 0) & (nodes["s_out_t"] > 0)]   # exported in at least one year
            fit = metrics.power_law_fit(d)
            ax.scatter(d["k_out"], d["s_out_t"], s=12, color=SERIES[0], alpha=0.6, linewidths=0)
            kk = np.logspace(np.log10(d["k_out"].min()), np.log10(d["k_out"].max()), 50)
            ax.plot(kk, fit["prefactor_t"] * kk ** fit["alpha"], color=SERIES[1], linewidth=2)
            med = (d["k_out"].median(), d["s_out_t"].median())
            ax.scatter(*med, s=70, color=INK, edgecolors=SURFACE, linewidths=1.5, zorder=5)
            ax.set_xscale("log"); ax.set_yscale("log")
            ax.text(0.04, 0.95, f"alpha = {fit['alpha']:.2f}\nn = {fit['n']}", transform=ax.transAxes,
                    va="top", fontsize=9, color=INK2)
            if r == 0:
                ax.set_title(label, color=INK, fontsize=11, loc="left")
            if c == 0:
                ax.set_ylabel(f"{CROP_LABEL[crop]}: exports s_out, t", color=INK2)
            if r == 1:
                ax.set_xlabel("Export partners k_out", color=INK2)
            rows.append({"crop": crop, "period": label, **fit, "median_k_out": med[0], "median_s_out_t": med[1]})
    fig.text(0.01, 0.005, "Orange line: least-squares fit s_out = c k_out^alpha. Black dot: component-wise median. "
             "Nodes that exported in at least one year of the period.", color=INK2, fontsize=8)
    fig.tight_layout(rect=(0, 0.02, 1, 1))
    fig.savefig(path, dpi=200)
    plt.close(fig)
    return path, pd.DataFrame(rows)


# --------------------------------------------------------------------------
# Figure 5


def figure5(inp, early: tuple, late: tuple, path: Path) -> tuple[Path, pd.DataFrame]:
    from . import pipeline

    periods = {early[0]: early[1], late[0]: late[1]}
    ssr = pipeline.ssr_by_period(inp, periods)
    m49 = dict(zip(inp.countries["fao"], inp.countries["m49"].astype(str).str.zfill(3)))
    fbs_m49 = _fbs_m49()
    ssr["m49"] = ssr["fao"].map(lambda c: m49.get(c, fbs_m49.get(c)))
    wide = ssr.pivot_table(index="m49", columns="period", values="ssr")
    wide["change"] = wide[late[0]] - wide[early[0]]
    world = countries_outline()
    world = world[world["NAME"] != "Antarctica"].merge(wide, left_on="m49", right_index=True, how="left")

    fig, axes = plt.subplots(3, 1, figsize=(9, 11.5), facecolor=SURFACE)
    ssr_levels = [0, 0.25, 0.5, 0.75, 0.95, 1.05, 1.5, 2, 4]
    ssr_cmap = plt.get_cmap("BrBG", len(ssr_levels))
    for ax, lab in zip(axes[:2], (early[0], late[0])):
        world.plot(ax=ax, column=lab, cmap=ssr_cmap, norm=BoundaryNorm(ssr_levels, ssr_cmap.N, extend="max"),
                   edgecolor=SURFACE, linewidth=0.3, legend=True,
                   legend_kwds={"shrink": 0.6, "label": "SSR, production / staple supply"},
                   missing_kwds={"color": NO_DATA, "edgecolor": SURFACE})
        ax.set_title(f"Self-sufficiency, {lab}", color=INK, fontsize=11, loc="left")
    ax = axes[2]
    lim = 0.6
    world.plot(ax=ax, column="change", cmap="RdBu", norm=TwoSlopeNorm(0, -lim, lim), edgecolor=SURFACE,
               linewidth=0.3, legend=True, legend_kwds={"shrink": 0.6, "label": "Change in SSR", "extend": "both"},
               missing_kwds={"color": NO_DATA, "edgecolor": SURFACE})
    ax.set_title(f"Change, {late[0]} minus {early[0]}", color=INK, fontsize=11, loc="left")
    for a in axes:
        a.set_axis_off()
        a.set_facecolor(SURFACE)
    fig.text(0.01, 0.005, "Cereals (excluding beer) plus 0.26 x starchy roots, FAOSTAT food balances. "
             "SSR 0.95-1.05 is marginal. Grey: no data.", color=INK2, fontsize=8)
    fig.tight_layout(rect=(0, 0.01, 1, 1))
    fig.savefig(path, dpi=200)
    plt.close(fig)
    return path, wide.reset_index()


def _fbs_m49() -> dict:
    """FAO area code -> M49 from the food balance archive, for countries absent from trade."""
    out = {}
    for name in ("fbs", "fbs_historic"):
        zp = config.DATA_DIR / f"{name}.zip"
        if not zp.exists():
            continue
        with zipfile.ZipFile(zp) as zf:
            area = [n for n in zf.namelist() if n.endswith("AreaCodes.csv")]
            if not area:
                continue
            with zf.open(area[0]) as fh:
                t = pd.read_csv(fh, dtype=str, encoding="utf-8", encoding_errors="replace")
        if {"Area Code", "M49 Code"} <= set(t.columns):
            for a, m in zip(t["Area Code"], t["M49 Code"]):
                out.setdefault(int(a), str(m).lstrip("'").zfill(3))
    return out


def make_all(inp, out_dir: Path = config.OUTPUT_DIR, periods: dict | None = None, skip_paleo: bool = False) -> list[Path]:
    periods = periods or {"1992-1996": (1992, 1996), "2005-2009": (2005, 2009), "2019-2023": (2019, 2023)}
    out_dir.mkdir(parents=True, exist_ok=True)
    made = [figure2(inp, out_dir / "fig2_exporters_2004_2006.png")]
    if not skip_paleo:
        made.append(figure3(out_dir / "fig3_disturbances.png"))
    p4, t4 = figure4(inp, periods, out_dir / "fig4_strength_degree.png")
    t4.to_csv(out_dir / "fig4_power_law.csv", index=False, float_format="%.6g")
    made.append(p4)
    labels = list(periods)
    p5, t5 = figure5(inp, (labels[0], periods[labels[0]]), (labels[-1], periods[labels[-1]]),
                     out_dir / "fig5_ssr_maps.png")
    t5.to_csv(out_dir / "fig5_ssr.csv", index=False, float_format="%.6g")
    made.append(p5)
    return made

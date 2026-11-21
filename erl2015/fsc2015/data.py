"""Load and cache the FAOSTAT inputs.

Every loader returns a tidy pandas DataFrame and validates what it reads.
Large bulk files are reduced once to small Parquet extracts in
``erl2015/data/`` (gitignored), so later runs read only the extract.
"""

from __future__ import annotations

import io
import shutil
import zipfile
from pathlib import Path

import pandas as pd

from . import config


class DataError(RuntimeError):
    """Raised when an input file is missing or fails validation."""


# --------------------------------------------------------------------------
# Download


def fetch_bulk(name: str, dest_dir: Path = config.DATA_DIR, force: bool = False) -> Path:
    """Download one FAOSTAT bulk ZIP to ``dest_dir`` and return its path."""
    import requests

    if name not in config.BULK_FILES:
        raise DataError(f"Unknown bulk file {name!r}; expected one of {sorted(config.BULK_FILES)}")
    dest_dir.mkdir(parents=True, exist_ok=True)
    dest = dest_dir / f"{name}.zip"
    if dest.exists() and not force:
        return dest
    url = config.BULK_BASE + config.BULK_FILES[name]
    partial = dest.with_suffix(".zip.partial")
    headers = {"User-Agent": "fsc2015-faostat-fetch"}
    with requests.get(url, stream=True, headers=headers, timeout=180) as resp:
        if resp.status_code >= 400:
            raise DataError(f"Download refused ({resp.status_code}): {url}")
        with partial.open("wb") as handle:
            for chunk in resp.iter_content(chunk_size=8 * 1024 * 1024):
                if chunk:
                    handle.write(chunk)
    if not zipfile.is_zipfile(partial):
        partial.unlink(missing_ok=True)
        raise DataError(f"Downloaded file is not a ZIP: {url}")
    shutil.move(partial, dest)
    return dest


def _main_csv(zf: zipfile.ZipFile) -> str:
    names = [n for n in zf.namelist() if n.endswith("All_Data_(Normalized).csv")]
    if len(names) != 1:
        raise DataError(f"Expected one All_Data CSV in archive, found {names}")
    return names[0]


def _require(df: pd.DataFrame, cols, what: str) -> None:
    missing = [c for c in cols if c not in df.columns]
    if missing:
        raise DataError(f"{what}: missing columns {missing}; found {list(df.columns)}")


def _is_country(code: pd.Series) -> pd.Series:
    return (code < config.MAX_COUNTRY_CODE) & ~code.isin(config.AGGREGATE_AREAS)


# --------------------------------------------------------------------------
# Trade


TRADE_COLS = {
    "Reporter Country Code": "reporter",
    "Reporter Country Code (M49)": "reporter_m49",
    "Reporter Countries": "reporter_name",
    "Partner Country Code": "partner",
    "Partner Country Code (M49)": "partner_m49",
    "Partner Countries": "partner_name",
    "Item Code": "item",
    "Element Code": "element",
    "Year": "year",
    "Unit": "unit",
    "Value": "value",
}


def extract_trade(zip_path: Path = config.TRADE_ZIP, out: Path | None = None,
                  chunksize: int = 2_000_000, verbose: bool = True) -> Path:
    """Reduce the FAOSTAT detailed trade matrix to wheat and rice quantities.

    The 8 GB CSV is streamed in chunks. Only quantity rows for the items in
    ``config.TRADE_ITEMS`` are kept. The result is written to Parquet.
    """
    out = out or config.DATA_DIR / "trade_wheat_rice.parquet"
    if not zip_path.exists():
        raise DataError(f"Trade archive not found: {zip_path}. Run main/fetch_faostat_bulk.py first.")
    items = {i for crop in config.TRADE_ITEMS.values() for i in crop}
    elements = {config.EXPORT_ELEMENT, config.IMPORT_ELEMENT}
    parts = []
    with zipfile.ZipFile(zip_path) as zf, zf.open(config.TRADE_CSV_IN_ZIP) as raw:
        reader = pd.read_csv(io.TextIOWrapper(raw, encoding="utf-8", errors="replace"), usecols=list(TRADE_COLS),
                             chunksize=chunksize, dtype=str)
        for k, chunk in enumerate(reader):
            chunk = chunk.rename(columns=TRADE_COLS)
            item = pd.to_numeric(chunk["item"], errors="coerce")
            element = pd.to_numeric(chunk["element"], errors="coerce")
            keep = item.isin(items) & element.isin(elements)
            if keep.any():
                parts.append(chunk.loc[keep])
            if verbose and k % 10 == 0:
                print(f"  trade chunk {k}: kept {sum(len(p) for p in parts):,} rows", flush=True)
    if not parts:
        raise DataError("No wheat or rice quantity rows found in the trade archive.")
    df = pd.concat(parts, ignore_index=True)
    for col in ("reporter", "partner", "item", "element", "year"):
        df[col] = pd.to_numeric(df[col], errors="raise").astype("int64")
    df["value"] = pd.to_numeric(df["value"], errors="coerce")
    for col in ("reporter_m49", "partner_m49"):
        df[col] = df[col].str.lstrip("'")
    out.parent.mkdir(parents=True, exist_ok=True)
    df.to_parquet(out, index=False)
    return out


def load_trade(path: Path | None = None) -> pd.DataFrame:
    """Read the trade extract and validate it."""
    path = path or config.DATA_DIR / "trade_wheat_rice.parquet"
    if not path.exists():
        raise DataError(f"Trade extract not found: {path}. Run `python -m fsc2015 prepare`.")
    df = pd.read_parquet(path)
    _require(df, ["reporter", "partner", "item", "element", "year", "unit", "value"], "trade extract")
    units = set(df["unit"].dropna().unique())
    if not units <= {"t", "tonnes"}:
        raise DataError(f"trade extract: unexpected units {units}")
    df = df[_is_country(df["reporter"]) & _is_country(df["partner"])]
    df = df[df["reporter"] != df["partner"]]
    df = df[df["value"].notna() & (df["value"] > 0)]
    return df.reset_index(drop=True)


def country_table(trade: pd.DataFrame) -> pd.DataFrame:
    """FAO area code, M49 code and name for every country seen in trade."""
    a = trade[["reporter", "reporter_m49", "reporter_name"]].set_axis(["fao", "m49", "name"], axis=1)
    b = trade[["partner", "partner_m49", "partner_name"]].set_axis(["fao", "m49", "name"], axis=1)
    tab = pd.concat([a, b]).drop_duplicates("fao").sort_values("fao").reset_index(drop=True)
    tab["name"] = tab["name"].map(_repair_text)
    iso = _iso3_lookup()
    tab["iso3"] = tab["fao"].map(iso)
    return tab


def _repair_text(text):
    """Undo UTF-8 text that was decoded as Latin-1 (older extracts)."""
    if not isinstance(text, str):
        return text
    try:
        return text.encode("latin-1").decode("utf-8")
    except (UnicodeEncodeError, UnicodeDecodeError):
        return text


def _iso3_lookup() -> dict[int, str]:
    path = config.REPO_ROOT / "ancillary" / "country_list195_2012to2016.csv"
    if not path.exists():
        return {}
    tab = pd.read_csv(path, encoding="utf-8-sig")
    return dict(zip(tab["FAOST_CODE"].astype(int), tab["iso3"]))


# --------------------------------------------------------------------------
# Food balances and GDP


def _extract_rows(zip_path: Path, item_codes, element_codes, out: Path, item_col="Item Code",
                  element_col="Element Code") -> Path:
    with zipfile.ZipFile(zip_path) as zf, zf.open(_main_csv(zf)) as raw:
        df = pd.read_csv(io.TextIOWrapper(raw, encoding="utf-8", errors="replace"), dtype=str)
    _require(df, ["Area Code", item_col, element_col, "Year", "Value"], zip_path.name)
    item = pd.to_numeric(df[item_col], errors="coerce")
    element = pd.to_numeric(df[element_col], errors="coerce")
    df = df[item.isin(item_codes) & element.isin(element_codes)].copy()
    if df.empty:
        raise DataError(f"{zip_path.name}: no rows for items {item_codes} and elements {element_codes}")
    keep = {"Area Code": "fao", "Area": "name", item_col: "item", "Item": "item_name",
            element_col: "element", "Element": "element_name", "Year": "year", "Unit": "unit",
            "Value": "value"}
    df = df[[c for c in keep if c in df.columns]].rename(columns=keep)
    for col in ("fao", "item", "element", "year"):
        df[col] = pd.to_numeric(df[col], errors="raise").astype("int64")
    df["value"] = pd.to_numeric(df["value"], errors="coerce")
    df.to_parquet(out, index=False)
    return out


def extract_food_balances(force_download: bool = False) -> Path:
    """Cereals (excl. beer) and starchy roots balances, historic and current."""
    out = config.DATA_DIR / "food_balances.parquet"
    items = [config.FBS_CEREALS, config.FBS_ROOTS]
    elements = list(config.FBS_ELEMENTS.values())
    frames = []
    for name, source in (("fbs_historic", "historic"), ("fbs", "current")):
        zp = fetch_bulk(name, force=force_download)
        tmp = config.DATA_DIR / f"_{name}.parquet"
        _extract_rows(zp, items, elements, tmp)
        part = pd.read_parquet(tmp)
        part["source"] = source
        frames.append(part)
        tmp.unlink()
    df = pd.concat(frames, ignore_index=True)
    # The two series overlap in 2010-2013. Prefer the current methodology.
    df = df.sort_values("source").drop_duplicates(["fao", "item", "element", "year"], keep="first")
    df.to_parquet(out, index=False)
    return out


def extract_gdp(force_download: bool = False) -> Path:
    """GDP in constant US dollars from FAOSTAT Macro Indicators."""
    out = config.DATA_DIR / "gdp.parquet"
    zp = fetch_bulk("macro", force=force_download)
    with zipfile.ZipFile(zp) as zf, zf.open(_main_csv(zf)) as raw:
        df = pd.read_csv(io.TextIOWrapper(raw, encoding="utf-8", errors="replace"), dtype=str)
    _require(df, ["Area Code", "Item", "Element", "Year", "Value"], zp.name)
    is_gdp = df["Item"].str.strip().str.lower().eq("gross domestic product")
    df = df[is_gdp & (df["Element"].str.strip() == config.GDP_ELEMENT)].copy()
    if df.empty:
        raise DataError(f"{zp.name}: no rows for GDP element {config.GDP_ELEMENT!r}")
    element = config.GDP_ELEMENT
    df = df.rename(columns={"Area Code": "fao", "Area": "name", "Year": "year", "Value": "value",
                            "Unit": "unit"})[["fao", "name", "year", "unit", "value"]]
    df["fao"] = pd.to_numeric(df["fao"], errors="raise").astype("int64")
    df["year"] = pd.to_numeric(df["year"], errors="raise").astype("int64")
    df["value"] = pd.to_numeric(df["value"], errors="coerce")
    df["element"] = element
    df.to_parquet(out, index=False)
    return out


def load_food_balances(path: Path | None = None) -> pd.DataFrame:
    path = path or config.DATA_DIR / "food_balances.parquet"
    if not path.exists():
        raise DataError(f"Food balance extract not found: {path}. Run `python -m fsc2015 prepare`.")
    df = pd.read_parquet(path)
    _require(df, ["fao", "item", "element", "year", "unit", "value"], "food balance extract")
    scale = df["unit"].map({"1000 t": 1000.0, "t": 1.0})
    if scale.isna().any():
        raise DataError(f"food balance extract: unexpected units {set(df.loc[scale.isna(), 'unit'])}")
    df = df.assign(value=df["value"] * scale, unit="t")
    return df[_is_country(df["fao"])].reset_index(drop=True)


def load_gdp(path: Path | None = None) -> pd.DataFrame:
    path = path or config.DATA_DIR / "gdp.parquet"
    if not path.exists():
        raise DataError(f"GDP extract not found: {path}. Run `python -m fsc2015 prepare`.")
    df = pd.read_parquet(path)
    _require(df, ["fao", "year", "value"], "GDP extract")
    return df[_is_country(df["fao"])].reset_index(drop=True)


# --------------------------------------------------------------------------
# Small tables shipped with the package


def load_ldc(path: Path | None = None) -> pd.DataFrame:
    path = path or config.ANCILLARY_DIR / "un_ldc_list.csv"
    df = pd.read_csv(path, dtype={"m49": str}, comment="#")
    _require(df, ["m49", "name"], path.name)
    df["m49"] = df["m49"].str.zfill(3)
    if df["m49"].duplicated().any():
        raise DataError(f"{path.name}: duplicate M49 codes")
    return df


def load_ldc_exclusions(path: Path | None = None) -> pd.DataFrame:
    path = path or config.ANCILLARY_DIR / "ldc_summary_exclusions.csv"
    if not path.exists():
        return pd.DataFrame(columns=["m49", "name", "reason"])
    df = pd.read_csv(path, dtype={"m49": str}, comment="#")
    _require(df, ["m49", "name", "reason"], path.name)
    df["m49"] = df["m49"].str.zfill(3)
    return df


def load_scenario(name_or_path: str | Path) -> pd.DataFrame:
    """Read a ban scenario: one row per banned exporter, by M49 code.

    Columns: ``m49``, ``name``, and optional ``ban_fraction`` in (0, 1].
    """
    path = Path(name_or_path)
    if not path.suffix:
        path = config.SCENARIO_DIR / f"{name_or_path}.csv"
    if not path.exists():
        raise DataError(f"Scenario not found: {path}")
    df = pd.read_csv(path, dtype={"m49": str}, comment="#")
    _require(df, ["m49", "name"], path.name)
    df["m49"] = df["m49"].str.zfill(3)
    if "ban_fraction" not in df.columns:
        df["ban_fraction"] = 1.0
    f = pd.to_numeric(df["ban_fraction"], errors="coerce")
    if f.isna().any() or (f <= 0).any() or (f > 1).any():
        raise DataError(f"{path.name}: ban_fraction must be in (0, 1]")
    df["ban_fraction"] = f
    if df["m49"].duplicated().any():
        raise DataError(f"{path.name}: duplicate M49 codes")
    return df

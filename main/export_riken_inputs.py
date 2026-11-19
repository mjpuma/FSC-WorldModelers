#!/usr/bin/env python3
"""Build the RIKEN input package for the Food Shock Cascade.

Reads FAOSTAT production, the FAOSTAT bilateral trade matrix, and USDA PSD.
The Agrimate / sheaf-model checkout is an input source only. This script does
not write into that repository.

Run from anywhere:

    python3 main/export_riken_inputs.py

Refresh 2022-2024 later by passing the new FAOSTAT files:

    python3 main/export_riken_inputs.py \\
        --production /path/to/production.csv \\
        --trade /path/to/trade.csv
"""

from __future__ import annotations

import argparse
import zipfile
from pathlib import Path

import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parents[1]

YEAR_START = 2002
YEAR_END = 2024
BASELINE_START = 2018
BASELINE_END = 2021
EU_SHARE_YEARS = (2019, 2020, 2021)

HUBS = ("HKG", "ARE", "SGP", "NLD", "ITA")

# Rice stays milled. PSD "Rice, Milled" is already milled and is not scaled again.
RICE_PRODUCTION_ITEM = 27
RICE_PRODUCTION_FACTOR = 0.67
RICE_TRADE_FACTORS = {
    27: 0.67,
    28: 0.8375,  # 0.67 / 0.80
    29: 0.8375,
    31: 1.0,
    32: 1.0,
    38: 1.0,
}

PRIMARY_ITEM = {
    "rice_milled": 27,
    "wheat": 15,
    "maize": 56,
    "soybean": 236,
}
PRIMARY_FACTOR = {
    "rice_milled": RICE_PRODUCTION_FACTOR,
    "wheat": 1.0,
    "maize": 1.0,
    "soybean": 1.0,
}

PSD_COMMODITY = {
    "Wheat": "wheat",
    "Rice, Milled": "rice_milled",
    "Corn": "maize",
    "Oilseed, Soybean": "soybean",
}
PSD_ATTRIBUTES = (
    "Beginning Stocks",
    "Domestic Consumption",
    "Production",
    "Imports",
    "Exports",
)

# Same names as main/ProcessInputs_funcs.R :: psd_name_map and the Congo/Yemen fixes.
PSD_NAME_MAP = {
    "Bolivia": "Bolivia (Plurinational State of)",
    "Brunei": "Brunei Darussalam",
    "Burkina": "Burkina Faso",
    "Burma": "Myanmar",
    "Cabo Verde": "Cape Verde",
    "China": "China (mainland)",
    "Cote d'Ivoire": "Cote dIvoire",
    "Czechia": "Czech Republic",
    "Eswatini": "Swaziland",
    "Former Czechoslovakia": "Czechoslovakia",
    "Former Yugoslavia": "Yugoslav SFR",
    "Gambia, The": "Gambia",
    "Hong Kong": "Hong Kong SAR, China",
    "Iran": "Iran (Islamic Republic of)",
    "Ivory Coast": "Cote dIvoire",
    "Korea, North": "Democratic People's Republic of Korea",
    "Korea, South": "Republic of Korea",
    "Laos": "Lao People's Democratic Republic",
    "Macedonia": "The former Yugoslav Republic of Macedonia",
    "North Macedonia": "The former Yugoslav Republic of Macedonia",
    "Moldova": "Republic of Moldova",
    "Russia": "Russian Federation",
    "Syria": "Syrian Arab Republic",
    "Taiwan": "Taiwan",
    "Tanzania": "United Republic of Tanzania",
    "Union of Soviet Socialist Repu": "USSR",
    "United States": "United States of America",
    "Venezuela": "Venezuela (Bolivarian Republic of)",
    "Vietnam": "Viet Nam",
}

EU_MEMBER_NAMES = (
    "Austria", "Belgium", "Denmark", "Finland", "France", "Germany", "Greece",
    "Ireland", "Italy", "Luxembourg", "Netherlands", "Portugal", "Spain", "Sweden",
    "United Kingdom", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Estonia",
    "Hungary", "Latvia", "Lithuania", "Poland", "Romania", "Slovakia", "Slovenia",
)

# SHEAF leaves these M49 codes blank or parks China's code on the aggregate row.
M49_OVERRIDE = {
    "CHN": 156,
    "TWN": 158,
    "SDN": 729,
    "SSD": 728,
    "HKG": 344,
    "MAC": 446,
}
PSD_CODE_OVERRIDE = {
    "CHN": "CH",
    "TWN": "TW",
    "SDN": "SU",
    "SSD": "OD",
}

SPLITS = (
    {
        "old": 206,
        "new": (276, 277),
        "last_year": 2011,
        "share_years": (2012, 2013, 2014),
        "label": "Sudan (former)",
    },
    {
        "old": 186,
        "new": (272, 273),
        "last_year": 2005,
        "share_years": (2006, 2007, 2008),
        "label": "Serbia and Montenegro",
    },
)

SUCCESSOR_NOTES = {
    "SDN": (
        "FAO 206 Sudan (former) is split into SDN (276) and SSD (277) through 2011 "
        "using 2012-2014 production shares. PSD 'Sudan' stays on SDN and is not split."
    ),
    "SSD": (
        "Successor of FAO 206 from 2012, with the 2012-2014 production share of code 277. "
        "Kept separate from Sudan."
    ),
    "SRB": (
        "FAO 186 Serbia and Montenegro is split into SRB (272) and MNE (273) through 2005 "
        "using 2006-2008 production shares. PSD 'Serbia and Montenegro' uses the same shares."
    ),
    "MNE": (
        "Successor of FAO 186 from 2006, with the 2006-2008 production share of code 273. "
        "PSD often has no Montenegro row; those years stay missing unless the SCG split applies."
    ),
    "TWN": (
        "Kept separate from China (mainland), FAO 41. Aggregate China FAO 351 is dropped."
    ),
}


def parse_args():
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument("--production", type=Path, default=ROOT / "inputs/Production_Crops_Livestock_E_All_Data_(Normalized).csv")
    p.add_argument("--trade", type=Path, default=Path("/Users/mjp38/GitHub/FoodTradeNetwork/inputs/Trade_DetailedTradeMatrix_E_All_Data_(Normalized).csv"))
    p.add_argument("--psd", type=Path, default=Path("/Users/mjp38/GitHub/sheaf-model/data/usda_psd/psd_alldata.csv"))
    p.add_argument("--country-table", type=Path, default=Path("/Users/mjp38/GitHub/sheaf-model/data/faostat_network/country_conversion_table.csv"))
    p.add_argument("--fsc-countries", type=Path, default=ROOT / "ancillary/country_list195_2012to2016.csv")
    p.add_argument("--factors", type=Path, default=ROOT / "ancillary/mass_equivalent_factors.csv")
    p.add_argument("--out", type=Path, default=ROOT / "outputs/export_for_RIKEN")
    p.add_argument("--zip", type=Path, default=ROOT / "outputs/export_for_RIKEN.zip")
    p.add_argument("--year-start", type=int, default=YEAR_START)
    p.add_argument("--year-end", type=int, default=YEAR_END)
    p.add_argument("--baseline-start", type=int, default=BASELINE_START)
    p.add_argument("--baseline-end", type=int, default=BASELINE_END)
    p.add_argument("--net-reexports", action=argparse.BooleanOptionalAction, default=True)
    p.add_argument("--self-test", action="store_true", help="Run the in-script checks and exit.")
    return p.parse_args()


def is_aggregate(code):
    code = int(code)
    return code >= 350 or 261 <= code <= 269


def harmonize_psd_name(name):
    out = PSD_NAME_MAP.get(str(name), str(name))
    out = out.replace("Congo (Brazzaville)", "Congo")
    out = out.replace("Congo (Kinshasa)", "Democratic Republic of the Congo")
    out = out.replace("Yemen (Aden)", "Yemen Dem")
    out = out.replace("Yemen (Sanaa)", "Yemen Ar Rp")
    return out


def unit_to_tonnes(unit):
    text = str(unit).strip().lower()
    if text in {"t", "tonnes", "tonne", "mt"}:
        return 1.0
    if text in {"1000 t", "1000 tonnes", "(1000 mt)", "1000 mt"}:
        return 1000.0
    if text in {"(mt)",}:
        return 1.0
    return None


def load_trade_factors(path):
    """Wheat, maize, and soybean tonne factors from the ancillary table. Rice is fixed."""
    table = pd.read_csv(path)
    factors = {item: float(factor) for item, factor in RICE_TRADE_FACTORS.items()}
    crop_of = {item: "rice_milled" for item in RICE_TRADE_FACTORS}
    for row in table.itertuples(index=False):
        crop = str(row.crop)
        if crop == "rice":
            continue
        factor = float(row.tonne_factor)
        if factor <= 0:
            continue
        item = int(row.cropid)
        factors[item] = factor
        crop_of[item] = {"wheat": "wheat", "maize": "maize", "soybean": "soybean"}[crop]
    return factors, crop_of


def load_countries(fsc_path, sheaf_path):
    fsc = pd.read_csv(fsc_path)
    fsc["FAOST_CODE"] = fsc["FAOST_CODE"].astype(int)
    fsc["iso3"] = fsc["iso3"].astype(str).str.strip()
    fsc["Country"] = fsc["Country"].astype(str)

    sheaf = pd.read_csv(sheaf_path, encoding="utf-8-sig")
    sheaf["FAOSTAT"] = pd.to_numeric(sheaf["FAOSTAT"], errors="coerce")
    sheaf = sheaf.dropna(subset=["FAOSTAT"]).copy()
    sheaf["FAOSTAT"] = sheaf["FAOSTAT"].astype(int)
    sheaf["m49_raw"] = pd.to_numeric(sheaf["ISO code"], errors="coerce")
    sheaf["psd_code_raw"] = sheaf["USDA PSD"].apply(lambda v: "" if pd.isna(v) else str(v).strip())
    by_fao = sheaf.drop_duplicates("FAOSTAT").set_index("FAOSTAT")

    inverse = {}
    for psd_name, fsc_name in PSD_NAME_MAP.items():
        inverse.setdefault(fsc_name, psd_name)
    inverse["The former Yugoslav Republic of Macedonia"] = "North Macedonia"

    rows = []
    fao_to_iso = {}
    for rec in fsc.itertuples(index=False):
        fao = int(rec.FAOST_CODE)
        iso = rec.iso3
        fao_to_iso[fao] = iso
        m49 = None
        psd_code = ""
        if fao in by_fao.index:
            raw_m49 = by_fao.at[fao, "m49_raw"]
            if pd.notna(raw_m49) and int(raw_m49) > 0:
                m49 = int(raw_m49)
            psd_code = by_fao.at[fao, "psd_code_raw"]
            if psd_code.lower() in {"", "nan", "null"}:
                psd_code = ""
        if iso in M49_OVERRIDE and m49 is None:
            m49 = M49_OVERRIDE[iso]
        if iso in PSD_CODE_OVERRIDE:
            psd_code = PSD_CODE_OVERRIDE[iso]
        rows.append({
            "iso3": iso,
            "fao_code": fao,
            "m49": m49 if m49 is not None else pd.NA,
            "country_name": rec.Country,
            "psd_code": psd_code,
            "psd_name": inverse.get(rec.Country, rec.Country),
            "successor_note": SUCCESSOR_NOTES.get(iso, ""),
            "reexport_hub": int(iso in HUBS),
            "in_fsc195": 1,
        })
    lookup = pd.DataFrame(rows).sort_values("iso3").reset_index(drop=True)
    name_to_iso = dict(zip(fsc["Country"], fsc["iso3"]))
    code_to_iso = {}
    for rec in lookup.itertuples(index=False):
        if rec.psd_code:
            code_to_iso[rec.psd_code] = rec.iso3
    eu_iso = []
    for name in EU_MEMBER_NAMES:
        if name not in name_to_iso:
            raise SystemExit(f"EU member {name!r} is not in the FSC country list")
        eu_iso.append(name_to_iso[name])
    return lookup, fao_to_iso, name_to_iso, code_to_iso, eu_iso


def production_shares(prod, new_codes, share_years):
    sub = prod[prod["fao"].isin(new_codes) & prod["year"].isin(share_years)]
    totals = sub.groupby("fao")["mt"].sum()
    total = float(totals.sum())
    if total <= 0:
        return None
    return {int(code): float(totals.get(code, 0.0)) / total for code in new_codes}


def apply_split_production(prod, split, shares):
    mask = (prod["fao"] == split["old"]) & (prod["year"] <= split["last_year"])
    if not mask.any():
        return prod
    base = prod.loc[mask].copy()
    rest = prod.loc[~mask]
    pieces = []
    for code, share in shares.items():
        part = base.copy()
        part["fao"] = int(code)
        part["mt"] = base["mt"] * share
        pieces.append(part)
    return pd.concat([rest, *pieces], ignore_index=True)


def apply_split_edges(edges, split, shares, side):
    mask = (edges[side] == split["old"]) & (edges["year"] <= split["last_year"])
    if not mask.any():
        return edges
    base = edges.loc[mask].copy()
    rest = edges.loc[~mask]
    pieces = []
    for code, share in shares.items():
        part = base.copy()
        part[side] = int(code)
        part["mt"] = base["mt"] * share
        pieces.append(part)
    return pd.concat([rest, *pieces], ignore_index=True)


def shares_by_crop(prod, log):
    """Crop-specific successor shares. Production must already be in metric tons."""
    out = {}
    for split in SPLITS:
        for crop, sub in prod.groupby("crop"):
            shares = production_shares(sub, split["new"], split["share_years"])
            needs = ((sub["fao"] == split["old"]) & (sub["year"] <= split["last_year"])).any()
            if needs and shares is None:
                raise SystemExit(
                    f"Cannot split {split['label']} for {crop}: no successor production "
                    f"in {split['share_years'][0]}-{split['share_years'][-1]}."
                )
            if shares is None:
                continue
            out[(crop, split["label"])] = shares
            log.append(
                f"Split FAO {split['old']} ({split['label']}) for {crop} through {split['last_year']}: "
                + ", ".join(f"{code}={share:.4f}" for code, share in shares.items())
            )
    return out


def split_all_production(prod, share_map):
    for split in SPLITS:
        crops = []
        for crop, sub in prod.groupby("crop"):
            shares = share_map.get((crop, split["label"]))
            if shares is None:
                crops.append(sub)
            else:
                crops.append(apply_split_production(sub, split, shares))
        prod = pd.concat(crops, ignore_index=True)
    return prod


def split_all_edges(edges, share_map):
    if edges.empty:
        return edges
    for split in SPLITS:
        pieces = []
        for crop, sub in edges.groupby("crop"):
            shares = share_map.get((crop, split["label"]))
            if shares is None:
                pieces.append(sub)
                continue
            stepped = apply_split_edges(sub, split, shares, "reporter")
            stepped = apply_split_edges(stepped, split, shares, "partner")
            pieces.append(stepped)
        edges = pd.concat(pieces, ignore_index=True)
    return edges


def read_production(path, factors, crop_of, year_start, year_end, log):
    primary = set(PRIMARY_ITEM.values())
    pieces = []
    years_seen = {crop: set() for crop in PRIMARY_ITEM}
    units = set()
    usecols = ["Area Code", "Item Code", "Element", "Year", "Unit", "Value"]
    for chunk in pd.read_csv(path, usecols=usecols, encoding="latin-1", chunksize=1_000_000):
        chunk = chunk[chunk["Element"] == "Production"]
        chunk = chunk[chunk["Item Code"].isin(primary)]
        chunk = chunk[chunk["Year"].between(year_start, year_end)]
        if chunk.empty:
            continue
        units.update(str(u) for u in chunk["Unit"].dropna().unique())
        scale = chunk["Unit"].map(unit_to_tonnes)
        if scale.isna().any():
            bad = sorted(set(chunk.loc[scale.isna(), "Unit"].astype(str)))
            raise SystemExit(f"Unrecognised production unit: {bad}")
        chunk = chunk.copy()
        chunk["mt"] = chunk["Value"].astype(float) * scale * chunk["Item Code"].map(PRIMARY_FACTOR_BY_ITEM)
        chunk["crop"] = chunk["Item Code"].map(PRIMARY_CROP_BY_ITEM)
        chunk["fao"] = chunk["Area Code"].astype(int)
        chunk["year"] = chunk["Year"].astype(int)
        for crop, years in chunk.groupby("crop")["year"]:
            years_seen[crop].update(int(y) for y in years.unique())
        pieces.append(chunk[["crop", "fao", "year", "mt"]])
    if not pieces:
        raise SystemExit(f"No production rows in {path}")
    prod = pd.concat(pieces, ignore_index=True)
    prod = prod.groupby(["crop", "fao", "year"], as_index=False)["mt"].sum()
    log.append(f"Production units: {', '.join(sorted(units))}")
    log.append(f"Production rows after item filter: {len(prod)}")
    return prod, years_seen


PRIMARY_CROP_BY_ITEM = {item: crop for crop, item in PRIMARY_ITEM.items()}
PRIMARY_FACTOR_BY_ITEM = {PRIMARY_ITEM[crop]: PRIMARY_FACTOR[crop] for crop in PRIMARY_ITEM}


def read_trade(path, factors, crop_of, year_start, year_end, log):
    items = set(factors)
    usecols = [
        "Reporter Country Code", "Partner Country Code", "Item Code",
        "Element", "Year", "Unit", "Value",
    ]
    pieces = []
    years_seen = {crop: set() for crop in PRIMARY_ITEM}
    dropped_aggregate_mt = 0.0
    n_raw = 0
    for i, chunk in enumerate(pd.read_csv(path, usecols=usecols, encoding="latin-1", chunksize=1_000_000), start=1):
        element = chunk["Element"].astype(str).str.strip().str.casefold()
        chunk = chunk[element == "export quantity"]
        chunk = chunk[chunk["Item Code"].isin(items)]
        chunk = chunk[chunk["Year"].between(year_start, year_end)]
        chunk = chunk[np.isfinite(chunk["Value"]) & (chunk["Value"] > 0)]
        if chunk.empty:
            if i % 10 == 0:
                print(f"  trade chunk {i}", flush=True)
            continue
        scale = chunk["Unit"].map(unit_to_tonnes)
        if scale.isna().any():
            bad = sorted(set(chunk.loc[scale.isna(), "Unit"].astype(str)))
            raise SystemExit(f"Unrecognised trade unit: {bad}")
        chunk = chunk.copy()
        chunk["mt"] = chunk["Value"].astype(float) * scale * chunk["Item Code"].map(factors)
        chunk["crop"] = chunk["Item Code"].map(crop_of)
        chunk["reporter"] = pd.to_numeric(chunk["Reporter Country Code"], errors="coerce")
        chunk["partner"] = pd.to_numeric(chunk["Partner Country Code"], errors="coerce")
        chunk = chunk.dropna(subset=["reporter", "partner"])
        chunk["reporter"] = chunk["reporter"].astype(int)
        chunk["partner"] = chunk["partner"].astype(int)
        chunk["year"] = chunk["Year"].astype(int)
        agg = chunk["reporter"].map(is_aggregate) | chunk["partner"].map(is_aggregate)
        dropped_aggregate_mt += float(chunk.loc[agg, "mt"].sum())
        chunk = chunk.loc[~agg]
        if chunk.empty:
            continue
        n_raw += len(chunk)
        for crop, years in chunk.groupby("crop")["year"]:
            years_seen[crop].update(int(y) for y in years.unique())
        g = chunk.groupby(["crop", "year", "reporter", "partner"], as_index=False)["mt"].sum()
        pieces.append(g)
        if i % 5 == 0:
            print(f"  trade chunk {i}, kept item-rows {n_raw}", flush=True)
    if not pieces:
        edges = pd.DataFrame(columns=["crop", "year", "reporter", "partner", "mt"])
    else:
        edges = pd.concat(pieces, ignore_index=True)
        edges = edges.groupby(["crop", "year", "reporter", "partner"], as_index=False)["mt"].sum()
    log.append(f"Trade edges before country crosswalk: {len(edges)}")
    log.append(f"Trade tonnes dropped on FAO aggregate areas: {dropped_aggregate_mt:.3f}")
    return edges, years_seen


def map_edges(edges, fao_to_iso, log):
    if edges.empty:
        edges["exporter_iso3"] = []
        edges["importer_iso3"] = []
        return edges
    edges = edges.copy()
    edges["exporter_iso3"] = edges["reporter"].map(fao_to_iso)
    edges["importer_iso3"] = edges["partner"].map(fao_to_iso)
    missing = edges["exporter_iso3"].isna() | edges["importer_iso3"].isna()
    log.append(
        f"Trade tonnes dropped because a reporter or partner is outside the FSC list: "
        f"{float(edges.loc[missing, 'mt'].sum()):.3f}"
    )
    edges = edges.loc[~missing].copy()
    self_trade = edges["exporter_iso3"] == edges["importer_iso3"]
    log.append(f"Self-trade tonnes dropped: {float(edges.loc[self_trade, 'mt'].sum()):.3f}")
    edges = edges.loc[~self_trade]
    edges = edges.groupby(["crop", "year", "exporter_iso3", "importer_iso3"], as_index=False)["mt"].sum()
    return edges


def net_reexports(edges, enabled, log):
    """Scale each hub's export row so remaining exports equal max(exports - imports, 0)."""
    if edges.empty:
        edges["trade_mt_reported"] = []
        edges["trade_mt_net"] = []
        edges["reexport_removed_mt"] = []
        return edges
    edges = edges.rename(columns={"mt": "trade_mt_reported"}).copy()
    exp = edges.groupby(["crop", "year", "exporter_iso3"])["trade_mt_reported"].sum()
    imp = edges.groupby(["crop", "year", "importer_iso3"])["trade_mt_reported"].sum()
    edges["reexport_removed_mt"] = 0.0
    for hub in HUBS:
        exp_h = exp[exp.index.get_level_values("exporter_iso3") == hub]
        for (crop, year, _), exports in exp_h.items():
            imports = float(imp.get((crop, year, hub), 0.0))
            removed_total = min(float(exports), imports)
            if exports <= 0 or removed_total <= 0:
                continue
            mask = (
                (edges["crop"] == crop)
                & (edges["year"] == year)
                & (edges["exporter_iso3"] == hub)
            )
            edges.loc[mask, "reexport_removed_mt"] = (
                edges.loc[mask, "trade_mt_reported"] * (removed_total / float(exports))
            )
    if enabled:
        edges["trade_mt_net"] = edges["trade_mt_reported"] - edges["reexport_removed_mt"]
        log.append("Re-export netting is on for HKG, ARE, SGP, NLD, ITA.")
    else:
        edges["trade_mt_net"] = edges["trade_mt_reported"]
        log.append("Re-export netting is off. reexport_removed_mt still records the hub residual.")
    log.append(f"Re-export tonnes recorded: {float(edges['reexport_removed_mt'].sum()):.3f}")
    return edges


def read_psd(path, year_start, year_end, name_to_iso, code_to_iso, log):
    usecols = [
        "Commodity_Description", "Country_Code", "Country_Name",
        "Market_Year", "Attribute_Description", "Unit_Description", "Value",
    ]
    pieces = []
    for chunk in pd.read_csv(path, usecols=usecols, chunksize=800_000):
        chunk = chunk[chunk["Commodity_Description"].isin(PSD_COMMODITY)]
        chunk = chunk[chunk["Attribute_Description"].isin(PSD_ATTRIBUTES)]
        chunk = chunk[chunk["Market_Year"].between(year_start, year_end)]
        if chunk.empty:
            continue
        scale = chunk["Unit_Description"].map(unit_to_tonnes)
        if scale.isna().any():
            bad = sorted(set(chunk.loc[scale.isna(), "Unit_Description"].astype(str)))
            raise SystemExit(f"Unrecognised PSD unit: {bad}")
        chunk = chunk.copy()
        chunk["mt"] = chunk["Value"].astype(float) * scale
        chunk["crop"] = chunk["Commodity_Description"].map(PSD_COMMODITY)
        chunk["year"] = chunk["Market_Year"].astype(int)
        chunk["attribute"] = chunk["Attribute_Description"]
        chunk["psd_name"] = chunk["Country_Name"].astype(str)
        chunk["psd_code"] = chunk["Country_Code"].astype(str)
        pieces.append(chunk[["crop", "year", "attribute", "psd_name", "psd_code", "mt"]])
    if not pieces:
        raise SystemExit(f"No PSD rows in {path}")
    raw = pd.concat(pieces, ignore_index=True)
    raw = raw.groupby(["crop", "year", "attribute", "psd_name", "psd_code"], as_index=False)["mt"].sum()
    raw["fsc_name"] = raw["psd_name"].map(harmonize_psd_name)
    raw["iso3"] = raw["fsc_name"].map(name_to_iso)
    missed = raw["iso3"].isna()
    raw.loc[missed, "iso3"] = raw.loc[missed, "psd_code"].map(code_to_iso)
    special = raw["psd_name"].isin(["European Union", "Serbia and Montenegro"])
    unmatched = sorted(set(raw.loc[raw["iso3"].isna() & ~special, "psd_name"]))
    if unmatched:
        log.append("PSD names with no ISO3 match: " + ", ".join(unmatched))
    log.append(f"PSD country-attribute rows: {len(raw)}")
    return raw


def eu_weights(prod_mt, crop, year, members):
    same = [float(prod_mt.get((crop, iso, int(year)), 0.0)) for iso in members]
    if sum(same) > 0:
        total = sum(same)
        return [v / total for v in same], "fao_same_year"
    share_rows = []
    for y in EU_SHARE_YEARS:
        one = [float(prod_mt.get((crop, iso, int(y)), 0.0)) for iso in members]
        if sum(one) > 0:
            total = sum(one)
            share_rows.append([v / total for v in one])
    if not share_rows:
        return None, "unavailable"
    mean = [float(np.mean([row[i] for row in share_rows])) for i in range(len(members))]
    total = sum(mean)
    return [v / total for v in mean], "fao_2019_2021_mean"


def build_reserves(psd, prod_iso, lookup, eu_iso, years, log):
    isos = lookup["iso3"].tolist()
    fao_of = dict(zip(lookup["iso3"], lookup["fao_code"]))
    iso_of_fao = dict(zip(lookup["fao_code"], lookup["iso3"]))
    prod_fao = prod_iso.copy()
    prod_fao["fao"] = prod_fao["iso3"].map(fao_of)
    prod_mt = {
        (r.crop, r.iso3, int(r.year)): float(r.mt)
        for r in prod_iso.itertuples(index=False)
    }

    direct = {}
    country_rows = psd[
        psd["iso3"].notna() & ~psd["psd_name"].isin(["European Union", "Serbia and Montenegro"])
    ]
    for rec in country_rows.itertuples(index=False):
        direct[(rec.crop, rec.iso3, int(rec.year), rec.attribute)] = float(rec.mt)

    scg = psd[psd["psd_name"] == "Serbia and Montenegro"]
    for crop, sub in scg.groupby("crop"):
        shares = production_shares(prod_fao[prod_fao["crop"] == crop], (272, 273), (2006, 2007, 2008))
        if shares is None:
            if len(sub):
                raise SystemExit(
                    f"PSD has Serbia and Montenegro for {crop}, but 2006-2008 production shares are missing."
                )
            continue
        for rec in sub.itertuples(index=False):
            for code, share in shares.items():
                iso = iso_of_fao[int(code)]
                key = (crop, iso, int(rec.year), rec.attribute)
                # A national row, when USDA publishes one, is the country's own stock.
                if key not in direct:
                    direct[key] = float(rec.mt) * share
        log.append(
            "Split PSD Serbia and Montenegro for "
            + f"{crop}: "
            + ", ".join(f"{code}={share:.4f}" for code, share in shares.items())
        )

    eu = {
        (r.crop, int(r.year), r.attribute): float(r.mt)
        for r in psd[psd["psd_name"] == "European Union"].itertuples(index=False)
    }
    frames = []
    weight_notes = []
    for crop in PRIMARY_ITEM:
        for year in years:
            weights, source = eu_weights(prod_mt, crop, year, eu_iso)
            eu_attrs = {
                attr: eu[(crop, int(year), attr)]
                for attr in PSD_ATTRIBUTES
                if (crop, int(year), attr) in eu
            }
            apportion = weights is not None and "Beginning Stocks" in eu_attrs
            if apportion and source != "fao_same_year":
                weight_notes.append(f"{crop} {year}: EU weights from {source}")
            for iso in isos:
                beginning = direct.get((crop, iso, int(year), "Beginning Stocks"))
                consumption = direct.get((crop, iso, int(year), "Domestic Consumption"))
                origin = "direct" if beginning is not None else "missing"
                if apportion and iso in eu_iso:
                    slot = eu_iso.index(iso)
                    beginning = eu_attrs["Beginning Stocks"] * weights[slot]
                    origin = "eu_apportioned"
                    if "Domestic Consumption" in eu_attrs:
                        consumption = eu_attrs["Domestic Consumption"] * weights[slot]
                if beginning is None:
                    status, flag, beginning_out = "missing_psd_row", 1, np.nan
                elif origin == "eu_apportioned":
                    status, flag, beginning_out = "eu_apportioned", 0, float(beginning)
                elif float(beginning) == 0.0:
                    status, flag, beginning_out = "psd_true_zero", 0, 0.0
                else:
                    status, flag, beginning_out = "psd_reported", 0, float(beginning)
                frames.append({
                    "crop": crop,
                    "iso3": iso,
                    "year": int(year),
                    "beginning_stocks_mt": beginning_out,
                    "psd_status": status,
                    "PSD_Missing_Flag": flag,
                    "psd_consumption_mt": np.nan if consumption is None else float(consumption),
                })
    reserves = pd.DataFrame(frames)
    if weight_notes:
        log.append(f"EU weight fallback years: {len(weight_notes)}")
        log.extend(weight_notes[:12])
    return reserves


def add_imputation(reserves, prod_iso, trade, log):
    """Domestic use prefers PSD consumption. The FAO identity is the fallback."""
    prod = prod_iso.rename(columns={"mt": "production_mt"})
    years_with_prod = set(zip(prod_iso["crop"], prod_iso["year"].astype(int)))
    exp = trade.groupby(["crop", "year", "exporter_iso3"], as_index=False)["trade_mt_reported"].sum()
    exp = exp.rename(columns={"exporter_iso3": "iso3", "trade_mt_reported": "fao_exports"})
    imp = trade.groupby(["crop", "year", "importer_iso3"], as_index=False)["trade_mt_reported"].sum()
    imp = imp.rename(columns={"importer_iso3": "iso3", "trade_mt_reported": "fao_imports"})
    merged = reserves.merge(prod, on=["crop", "iso3", "year"], how="left")
    merged = merged.merge(exp, on=["crop", "iso3", "year"], how="left")
    merged = merged.merge(imp, on=["crop", "iso3", "year"], how="left")
    year_ok = [
        (crop, int(year)) in years_with_prod
        for crop, year in zip(merged["crop"], merged["year"])
    ]
    fao_identity = (
        merged["production_mt"].fillna(0)
        + merged["fao_imports"].fillna(0)
        - merged["fao_exports"].fillna(0)
    ).clip(lower=0)
    fao_identity = fao_identity.where(year_ok, np.nan)
    merged["domestic_use_mt"] = merged["psd_consumption_mt"].where(
        merged["psd_consumption_mt"].notna(), fao_identity
    )
    can_ratio = (
        (merged["PSD_Missing_Flag"] == 0)
        & merged["beginning_stocks_mt"].notna()
        & merged["domestic_use_mt"].gt(0)
    )
    merged["stock_to_use"] = np.where(
        can_ratio, merged["beginning_stocks_mt"] / merged["domestic_use_mt"], np.nan
    )
    donors = merged["psd_status"].isin(["psd_reported", "psd_true_zero"]) & can_ratio
    proxy = (
        merged.loc[donors]
        .groupby(["crop", "year"])["stock_to_use"]
        .median()
        .rename("stock_to_use_proxy")
    )
    merged = merged.drop(columns=["stock_to_use_proxy"], errors="ignore")
    merged = merged.merge(proxy, on=["crop", "year"], how="left")
    merged["reserves_imputed_mt"] = np.nan
    merged["imputation_method"] = ""
    missing = merged["PSD_Missing_Flag"] == 1
    no_use = missing & ~merged["domestic_use_mt"].gt(0)
    no_donor = missing & merged["domestic_use_mt"].gt(0) & merged["stock_to_use_proxy"].isna()
    filled = missing & merged["domestic_use_mt"].gt(0) & merged["stock_to_use_proxy"].notna()
    merged.loc[no_use, "imputation_method"] = "no_domestic_use"
    merged.loc[no_donor, "imputation_method"] = "no_donor"
    merged.loc[filled, "imputation_method"] = "median_stock_to_use"
    merged.loc[filled, "reserves_imputed_mt"] = (
        merged.loc[filled, "stock_to_use_proxy"] * merged.loc[filled, "domestic_use_mt"]
    )
    log.append(
        "Reserve rows filled with the median stock-to-use proxy: "
        + str(int(filled.sum()))
    )
    return merged


def balanced_production(prod_iso, lookup, years_seen, year_start, year_end):
    rows = []
    isos = lookup["iso3"].tolist()
    present = {(r.crop, r.iso3, int(r.year)): float(r.mt) for r in prod_iso.itertuples(index=False)}
    for crop, item in PRIMARY_ITEM.items():
        for year in range(year_start, year_end + 1):
            if year not in years_seen[crop]:
                continue
            for iso in isos:
                rows.append({
                    "crop": crop,
                    "iso3": iso,
                    "year": year,
                    "production_mt": present.get((crop, iso, year), 0.0),
                    "source_item": item,
                    "tonne_factor": PRIMARY_FACTOR[crop],
                })
    return pd.DataFrame(rows)


def coverage_rows(prod, reserves, trade, years_seen_prod, years_seen_trade, year_start, year_end):
    rows = []
    for crop in PRIMARY_ITEM:
        for year in range(year_start, year_end + 1):
            if year in years_seen_prod[crop]:
                sub = prod[(prod["crop"] == crop) & (prod["year"] == year)]
                rows.append(_cov(crop, "production", year, "ok", len(sub), sub["iso3"].nunique(), ""))
            else:
                rows.append(_cov(
                    crop, "production", year, "blocked_missing_faostat", 0, 0,
                    "FAOSTAT production file has no rows for this year.",
                ))
            if year in years_seen_trade[crop]:
                sub = trade[(trade["crop"] == crop) & (trade["year"] == year)] if len(trade) else trade
                n_cty = 0 if sub is None or len(sub) == 0 else len(set(sub["exporter_iso3"]) | set(sub["importer_iso3"]))
                rows.append(_cov(crop, "bilateral_trade", year, "ok", 0 if sub is None else len(sub), n_cty, ""))
            else:
                rows.append(_cov(
                    crop, "bilateral_trade", year, "blocked_missing_faostat", 0, 0,
                    "FAOSTAT trade file has no Export Quantity rows for this year.",
                ))
            sub = reserves[(reserves["crop"] == crop) & (reserves["year"] == year)]
            fao_missing = year not in years_seen_prod[crop]
            status = "psd_only" if fao_missing else "ok"
            note = "Beginning stocks from USDA PSD. FAOSTAT production and trade are absent for this year." if fao_missing else ""
            rows.append(_cov(crop, "reserves", year, status, len(sub), sub["iso3"].nunique(), note))
    return pd.DataFrame(rows)


def _cov(crop, series, year, status, n_rows, n_countries, notes):
    return {
        "crop": crop,
        "series": series,
        "year": int(year),
        "status": status,
        "n_rows": int(n_rows),
        "n_countries": int(n_countries),
        "notes": notes,
    }


def write_crop_files(frame, out_dir, stem, columns, year_start, year_end):
    out_dir.mkdir(parents=True, exist_ok=True)
    window = frame[frame["year"].between(year_start, year_end)]
    for crop in PRIMARY_ITEM:
        sub = window[window["crop"] == crop][columns].copy()
        if "exporter_iso3" in sub.columns:
            sub = sub.sort_values(["year", "exporter_iso3", "importer_iso3"])
        else:
            sub = sub.sort_values(["iso3", "year"])
        path = out_dir / f"{crop}_{stem}.csv"
        sub.to_csv(path, index=False, na_rep="")


def write_readme(path, args, log, coverage):
    blocked = coverage[coverage["status"] == "blocked_missing_faostat"]
    blocked_years = sorted(set(blocked["year"].tolist())) if len(blocked) else []
    text = f"""# FSC inputs for the RIKEN flood-loss collaboration

Metric tons. Rice is milled equivalent. Country keys are ISO3. The panel is the
195-country FSC list (`ancillary/country_list195_2012to2016.csv`).

This package was built by `main/export_riken_inputs.py`. Agrimate / sheaf-model
was read and not modified.

## Units

| Crop | Production | Trade | USDA PSD stocks |
|---|---|---|---|
| `rice_milled` | item 27 times 0.67 | item 27 times 0.67; items 28 and 29 times 0.8375 (0.67/0.80); items 31, 32, and 38 times 1 | `Rice, Milled`, already milled, times 1 |
| `wheat` | item 15 | grain and product factors in `ancillary/mass_equivalent_factors.csv` | `Wheat` |
| `maize` | item 56 | grain and flour factors in that table; oil and green maize excluded | `Corn` |
| `soybean` | item 236 | beans only; oil and cake excluded | `Oilseed, Soybean` |

PSD values are published in 1000 MT and multiplied by 1000. FAOSTAT production
and export quantity are tonnes. PSD `Market_Year` is the stock year. FAOSTAT
`Year` is the calendar year. They are joined on the year number and are not
shifted to a common marketing year.

## Country codes

`country_lookup_ISO3_FAO.csv` uses the SHEAF conversion table for M49 and USDA
codes, then keeps the FSC FAO code and ISO3. FAO area codes of 350 and above,
and codes 261-269, are dropped. That removes aggregate China (351). China
(mainland) stays FAO 41 / CHN, and Taiwan stays FAO 214 / TWN.

Where the SHEAF table left M49 blank, the lookup fills CHN 156, TWN 158,
SDN 729, SSD 728, HKG 344, and MAC 446.

Sudan (former), FAO 206, is split into SDN and SSD through 2011 using each
crop's 2012-2014 production shares. Serbia and Montenegro, FAO 186, is split
into SRB and MNE through 2005 using 2006-2008 production shares. The same
shares split a PSD `Serbia and Montenegro` stock row. PSD `Sudan` is left on
SDN. The European Union is one PSD row (code E4). Its beginning stocks and
domestic consumption are apportioned to the EU28-with-UK member list in
`ProcessInputs_funcs.R`, by that year's FAO production. Malta is in the
195-country list and is not in that member list, so Malta keeps its own PSD
row. For a year with no FAO production, member shares are the mean of
2019-2021. A year whose member production sums to zero is not apportioned.

## Re-exports

Hubs are HKG, ARE, SGP, NLD, and ITA. For each hub, crop, and year,
re-exports are `min(exports, imports)`. Each of the hub's export edges is
scaled by `(exports - re-exports) / exports`. `reexport_removed_mt` is the
tonnes removed from that edge. The removed flow is not given back to an
upstream origin. `--net-reexports` (the default) writes the scaled value in
`trade_mt_net`. `--no-net-reexports` leaves `trade_mt_net` equal to
`trade_mt_reported` and still fills `reexport_removed_mt`.

Self-trade is dropped. Flows whose reporter or partner is outside the 195
countries are dropped after the successor split.

## Stocks

`beginning_stocks_mt` is the raw PSD beginning stock, blank when USDA has no
row. `PSD_Missing_Flag` is 1 only for `missing_psd_row`. A reported zero is
`psd_true_zero` and the flag stays 0. EU shares are `eu_apportioned` and the
flag stays 0.

`stock_to_use` is beginning stocks divided by domestic use when domestic use
is positive. Domestic use is PSD domestic consumption when USDA reports it
(including an EU share), otherwise `max(production + imports - exports, 0)`
from the reported, pre-netting trade flows.

`stock_to_use_proxy` is the same-year median stock-to-use of countries with a
reported or true-zero stock and positive domestic use. EU-apportioned rows are
not donors. `reserves_imputed_mt` is that median times domestic use, and only
on rows with `PSD_Missing_Flag` 1 and positive domestic use. The raw stock
column is not overwritten.

## Files

`baseline_2018_2021/` is the {args.baseline_start}-{args.baseline_end} slice.
`annual_networks_2002_2024/` is {args.year_start}-{args.year_end}. Production
and trade years with no FAOSTAT rows are omitted from the CSVs and marked
`blocked_missing_faostat` in `COVERAGE.csv`. Reserve rows are written for
every year in the window, including PSD-only years.

Blocked FAOSTAT years in this build: {blocked_years if blocked_years else "none"}.

## Rebuild

```bash
Rscript main/export_riken_inputs.R \\
  --production /path/to/Production_....csv \\
  --trade /path/to/Trade_DetailedTradeMatrix_....csv \\
  --psd /path/to/psd_alldata.csv
```

Paths default to the FAOSTAT files already on this machine, the FoodTradeNetwork
trade matrix, and `sheaf-model/data/usda_psd/psd_alldata.csv`. A newer
production file and trade matrix fill 2022-2024 without a code change.

## Build notes

"""
    text += "\n".join(f"- {line}" for line in log)
    text += "\n"
    path.write_text(text)


def zip_directory(src, dest):
    dest.parent.mkdir(parents=True, exist_ok=True)
    if dest.exists():
        dest.unlink()
    with zipfile.ZipFile(dest, "w", compression=zipfile.ZIP_DEFLATED) as zf:
        for path in sorted(src.rglob("*")):
            if path.is_file():
                zf.write(path, path.relative_to(src).as_posix())


def run_internal_checks():
    prod = pd.DataFrame({
        "crop": ["wheat", "wheat", "wheat"],
        "fao": [206, 276, 277],
        "year": [2010, 2012, 2012],
        "mt": [100.0, 80.0, 20.0],
    })
    # Share years need 2012-2014; one year is enough when the others are absent
    # only if the sum is positive. The helper sums whatever is present.
    shares = production_shares(prod, (276, 277), (2012, 2013, 2014))
    assert abs(shares[276] - 0.8) < 1e-12 and abs(shares[277] - 0.2) < 1e-12
    split = apply_split_production(prod, SPLITS[0], shares)
    sudan = split[(split["fao"] == 276) & (split["year"] == 2010)]["mt"].sum()
    south = split[(split["fao"] == 277) & (split["year"] == 2010)]["mt"].sum()
    assert abs(sudan - 80) < 1e-9 and abs(south - 20) < 1e-9, (sudan, south)

    edges = pd.DataFrame({
        "crop": ["wheat", "wheat", "wheat"],
        "year": [2018, 2018, 2018],
        "exporter_iso3": ["HKG", "HKG", "USA"],
        "importer_iso3": ["AAA", "BBB", "HKG"],
        "mt": [100.0, 50.0, 90.0],
    })
    net = net_reexports(edges, True, [])
    hkg = net[net["exporter_iso3"] == "HKG"].sort_values("importer_iso3")
    # exports 150, imports 90, remaining fraction 0.4
    assert abs(hkg["trade_mt_net"].sum() - 60) < 1e-6
    assert abs(hkg["reexport_removed_mt"].sum() - 90) < 1e-6
    usa = net[net["exporter_iso3"] == "USA"].iloc[0]
    assert abs(usa.trade_mt_net - 90) < 1e-9 and abs(usa.reexport_removed_mt) < 1e-9
    gross = net_reexports(edges, False, [])
    assert abs(gross["trade_mt_net"].sum() - gross["trade_mt_reported"].sum()) < 1e-9
    assert abs(gross.loc[gross["exporter_iso3"] == "HKG", "reexport_removed_mt"].sum() - 90) < 1e-6
    assert is_aggregate(351) and is_aggregate(265) and not is_aggregate(41) and not is_aggregate(214)
    assert abs(RICE_TRADE_FACTORS[28] - 0.67 / 0.80) < 1e-12
    print("OK: export internal checks")


def main():
    args = parse_args()
    run_internal_checks()
    if args.self_test:
        return
    for label, path in (
        ("production", args.production),
        ("trade", args.trade),
        ("psd", args.psd),
        ("country table", args.country_table),
        ("FSC countries", args.fsc_countries),
        ("factors", args.factors),
    ):
        if not path.exists():
            raise SystemExit(f"Missing {label}: {path}")

    log = []
    print("Loading countries", flush=True)
    lookup, fao_to_iso, name_to_iso, code_to_iso, eu_iso = load_countries(
        args.fsc_countries, args.country_table
    )
    factors, crop_of = load_trade_factors(args.factors)
    log.append(f"Countries in the lookup: {len(lookup)}")
    log.append(f"Re-export netting: {bool(args.net_reexports)}")

    print("Reading production", flush=True)
    prod, years_prod = read_production(
        args.production, factors, crop_of, args.year_start, args.year_end, log
    )
    share_map = shares_by_crop(prod, log)
    prod = split_all_production(prod, share_map)
    prod = prod[prod["fao"].map(lambda code: code in fao_to_iso)].copy()
    prod["iso3"] = prod["fao"].map(fao_to_iso)
    prod_iso = prod.groupby(["crop", "iso3", "year"], as_index=False)["mt"].sum()

    print("Reading trade (chunked)", flush=True)
    edges, years_trade = read_trade(
        args.trade, factors, crop_of, args.year_start, args.year_end, log
    )
    edges = split_all_edges(edges, share_map)
    edges = map_edges(edges, fao_to_iso, log)
    trade = net_reexports(edges, args.net_reexports, log)

    print("Reading PSD", flush=True)
    psd = read_psd(args.psd, args.year_start, args.year_end, name_to_iso, code_to_iso, log)
    years = list(range(args.year_start, args.year_end + 1))
    print("Building reserves", flush=True)
    reserves = build_reserves(psd, prod_iso, lookup, eu_iso, years, log)
    print("Imputing missing stocks", flush=True)
    reserves = add_imputation(reserves, prod_iso, trade, log)

    production = balanced_production(prod_iso, lookup, years_prod, args.year_start, args.year_end)
    coverage = coverage_rows(
        production, reserves, trade, years_prod, years_trade, args.year_start, args.year_end
    )

    out = args.out
    out.mkdir(parents=True, exist_ok=True)
    lookup.to_csv(out / "country_lookup_ISO3_FAO.csv", index=False, na_rep="")
    coverage.to_csv(out / "COVERAGE.csv", index=False)
    (out / "BUILD_LOG.txt").write_text("\n".join(log) + "\n")

    prod_cols = ["iso3", "year", "production_mt", "source_item", "tonne_factor"]
    res_cols = [
        "iso3", "year", "beginning_stocks_mt", "psd_status", "PSD_Missing_Flag",
        "domestic_use_mt", "stock_to_use", "stock_to_use_proxy",
        "reserves_imputed_mt", "imputation_method",
    ]
    trade_cols = [
        "year", "exporter_iso3", "importer_iso3",
        "trade_mt_reported", "trade_mt_net", "reexport_removed_mt",
    ]
    annual = out / "annual_networks_2002_2024"
    base = out / "baseline_2018_2021"
    for folder, y0, y1, prod_stem, res_stem, tr_stem in (
        (annual, args.year_start, args.year_end,
         f"production_{args.year_start}_{args.year_end}_mt",
         f"reserves_{args.year_start}_{args.year_end}_mt",
         f"bilateral_trade_{args.year_start}_{args.year_end}_mt"),
        (base, args.baseline_start, args.baseline_end,
         f"production_{args.baseline_start}_{args.baseline_end}_mt",
         f"reserves_{args.baseline_start}_{args.baseline_end}_mt",
         f"bilateral_trade_{args.baseline_start}_{args.baseline_end}_mt"),
    ):
        write_crop_files(production, folder, prod_stem, prod_cols, y0, y1)
        write_crop_files(reserves, folder, res_stem, res_cols, y0, y1)
        write_crop_files(trade, folder, tr_stem, trade_cols, y0, y1)

    write_readme(out / "README_FSC_Inputs.md", args, log, coverage)
    print("Zipping", flush=True)
    zip_directory(out, args.zip)
    print(f"Wrote {out}")
    print(f"Wrote {args.zip}")
    print(coverage.groupby(["series", "status"]).size().to_string())


if __name__ == "__main__":
    main()

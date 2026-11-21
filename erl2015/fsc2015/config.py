"""Fixed settings for the Python implementation of Puma et al. (2015).

Commodity lists and factors follow this repository's FSC tonnage build
(`ancillary/mass_equivalent_factors.csv`), so wheat is in wheat-grain
equivalent and rice is in milled-rice equivalent throughout.
"""

from __future__ import annotations

from pathlib import Path

PACKAGE_DIR = Path(__file__).resolve().parent
ERL_DIR = PACKAGE_DIR.parent
REPO_ROOT = ERL_DIR.parent
DATA_DIR = ERL_DIR / "data"
SCENARIO_DIR = ERL_DIR / "scenarios"
ANCILLARY_DIR = ERL_DIR / "ancillary"
OUTPUT_DIR = REPO_ROOT / "outputs" / "erl2015"

TRADE_ZIP = REPO_ROOT / "inputs" / "faostat_bulk_cache" / "trade.zip"
TRADE_CSV_IN_ZIP = "Trade_DetailedTradeMatrix_E_All_Data_(Normalized).csv"

BULK_BASE = "https://bulks-faostat.fao.org/production/"
BULK_FILES = {
    "fbs": "FoodBalanceSheets_E_All_Data_(Normalized).zip",
    "fbs_historic": "FoodBalanceSheetsHistoric_E_All_Data_(Normalized).zip",
    "macro": "Macro-Statistics_Key_Indicators_E_All_Data_(Normalized).zip",
}

# FAOSTAT trade item code -> tonne factor into the crop's reference unit.
TRADE_ITEMS = {
    "wheat": {
        15: 1.0,        # Wheat
        16: 1.282051,   # Wheat and meslin flour, 0.78 extraction
        18: 1.282051,   # Uncooked pasta
        19: 1.0,        # Germ of wheat
        20: 0.9,        # Bread
        21: 1.111111,   # Bulgur, 0.90 extraction
        22: 1.282051,   # Pastry
    },
    "rice": {
        27: 0.67,       # Rice (paddy) to milled
        28: 0.8375,     # Husked rice to milled (0.67 / 0.80)
        29: 0.8375,     # Rice, milled (husked), treated as husked
        31: 1.0,        # Rice, milled
        32: 1.0,        # Rice, broken
        38: 1.0,        # Flour of rice
    },
}
UNITS = {"wheat": "t wheat-grain equivalent", "rice": "t milled rice equivalent"}

EXPORT_ELEMENT = 5910   # Export quantity
IMPORT_ELEMENT = 5610   # Import quantity

# FAO area codes that are aggregates of other reporters. 351 is "China"
# (mainland 41 + Hong Kong 96 + Macao 128 + Taiwan 214).
AGGREGATE_AREAS = {351}
MAX_COUNTRY_CODE = 5000  # codes >= 5000 are regions

# Food balance items for staple food supply, as in the 2015 paper.
FBS_CEREALS = 2905        # Cereals - Excluding Beer
FBS_ROOTS = 2907          # Starchy Roots
ROOTS_CEREAL_EQUIV = 0.26
FBS_ELEMENTS = {
    "production": 5511,
    "import": 5611,
    "export": 5911,
    "domestic_supply": 5301,
}

GDP_ELEMENT = "Value US$, 2015 prices"  # million USD, constant 2015 prices

# Default analysis windows. The early window repeats the paper's first
# period; the late window is the most recent five years in the trade file.
DEFAULT_PERIODS = {"1992-1996": (1992, 1996), "2019-2023": (2019, 2023)}

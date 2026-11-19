# FSC network inputs for flood-loss analysis

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

`baseline_2018_2021/` is the 2018-2021 slice.
`annual_networks_2002_2024/` is 2002-2024. Production
and trade years with no FAOSTAT rows are omitted from the CSVs and marked
`blocked_missing_faostat` in `COVERAGE.csv`. Reserve rows are written for
every year in the window, including PSD-only years.

Blocked FAOSTAT years in this build: none.

## Rebuild

```bash
Rscript main/export_riken_inputs.R \
  --production /path/to/Production_....csv \
  --trade /path/to/Trade_DetailedTradeMatrix_....csv \
  --psd /path/to/psd_alldata.csv
```

Paths default to the FAOSTAT files already on this machine, the FoodTradeNetwork
trade matrix, and `sheaf-model/data/usda_psd/psd_alldata.csv`. A newer
production file and trade matrix fill 2022-2024 without a code change.

## Build notes

- Countries in the lookup: 195
- Re-export netting: True
- Production units: t
- Production rows after item filter: 15041
- Split FAO 206 (Sudan (former)) for maize through 2011: 276=0.1992, 277=0.8008
- Split FAO 206 (Sudan (former)) for rice_milled through 2011: 276=1.0000, 277=0.0000
- Split FAO 206 (Sudan (former)) for wheat through 2011: 276=1.0000, 277=0.0000
- Split FAO 186 (Serbia and Montenegro) for maize through 2005: 272=0.9993, 273=0.0007
- Split FAO 186 (Serbia and Montenegro) for soybean through 2005: 272=1.0000, 273=0.0000
- Split FAO 186 (Serbia and Montenegro) for wheat through 2005: 272=0.9993, 273=0.0007
- Trade edges before country crosswalk: 273764
- Trade tonnes dropped on FAO aggregate areas: 0.000
- Trade tonnes dropped because a reporter or partner is outside the FSC list: 32488902.765
- Self-trade tonnes dropped: 394005.296
- Re-export netting is on for HKG, ARE, SGP, NLD, ITA.
- Re-export tonnes recorded: 172155320.579
- PSD names with no ISO3 match: Angola, Reunion
- PSD country-attribute rows: 48575
- Split PSD Serbia and Montenegro for maize: 272=0.9993, 273=0.0007
- Split PSD Serbia and Montenegro for soybean: 272=1.0000, 273=0.0000
- Split PSD Serbia and Montenegro for wheat: 272=0.9993, 273=0.0007
- Reserve rows filled with the median stock-to-use proxy: 4878

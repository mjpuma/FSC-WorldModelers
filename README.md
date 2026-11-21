<p align="center">
  <img src="clean_vector_logo_on_white_background_a_circular.png" width="400" alt="Food Shock Cascade (FSC) model logo">
</p>

# Food Shock Cascade Model

The Food Shock Cascade (FSC) model simulates how agricultural production shocks and export restrictions propagate through country-level trade networks, one crop at a time. A production loss or an export ban changes supply. Countries then draw stocks, cut domestic use, and adjust trade along links that already exist. The result is a new pattern of consumption and trade. There are no households, no prices in the cascade, and no substitution between crops.

FSC began with Puma, Bose, Chon, and Cook, 2015, *Assessing the evolving fragility of the global food system*, Environmental Research Letters, doi:10.1088/1748-9326/10/2/024007. That paper built the country-level wheat and rice trade networks and asked what happens to importers when a block of exporters stops selling. A Python implementation of that model, updated for more recent data, is in [`erl2015/`](erl2015/). See [Lineage](#lineage).

The published model is Heslin, Puma, Marchand, and coauthors, 2020, *Simulating the cascading effects of an extreme agricultural production shock*, Frontiers in Sustainable Food Systems, doi:10.3389/fsufs.2020.00026. That paper is the proportional-trade and reserves-trade pair, run in kilocalories. The earlier statement of the rules is Marchand and coauthors, 2016, *Reserves and trade jointly determine exposure to food supply shocks*, Environmental Research Letters, doi:10.1088/1748-9326/11/9/095009, with code at https://github.com/pmarchand1/cereals-network-shocks.

This repository also holds a metric-ton build for 2002 to 2024, used to examine flood-related production shocks. That build is described under [Flood scenarios](#flood-scenarios). It does not replace the published kilocalorie scenarios.

Run every command from the repository root.

## Quick start

Published R cascade (PTA, scenario 1, half of beginning stocks usable):

```
Rscript main/Requirements.R
Rscript main/main.R 0 1 0.5
```

The first argument picks the mode: 0 is PTA, 1 is RTA, 2 is stock-then-split. The second is the scenario number and the third is alpha. Results go to `outputs/`. See [How to run a published scenario](#how-to-run-a-published-scenario).

Flood run on each year's own network. This needs the draft flood-shock table, which is kept local (see [Data that stays local](#data-that-stays-local)):

```
FSC_FLOOD_TABLE=/path/to/draft_flood_shock_table.xlsx Rscript main/run_flood_shocks.R
python3 main/plot_flood_report.py
```

Without the draft table, the placeholder flood comparison runs from tracked files:

```
Rscript main/run_allocation_comparison.R
```

Puma et al. 2015 export-ban model, in Python:

```
cd erl2015
pip install -r requirements.txt
python3 -m fsc2015 prepare
python3 -m fsc2015 run
python3 -m fsc2015 figures
```

See [`erl2015/README.md`](erl2015/README.md) for options and tests.

## Lineage

Each step below kept the country trade network of the one before it and added one mechanism.

| Step | Paper | What it added | Where it lives |
|---|---|---|---|
| 1. Export-ban accounting | Puma et al. 2015, ERL, doi:10.1088/1748-9326/10/2/024007 | Annual bilateral wheat and rice networks, network metrics, and continental export bans. Losses are counted with no reallocation (static) or with the remaining world exports handed to importers in descending GDP order (dynamic). No stocks and no iteration. | `erl2015/`, Python, FAOSTAT through 2024 |
| 2. Reserves and proportional cascade | Marchand et al. 2016, ERL, doi:10.1088/1748-9326/11/9/095009 | Production shocks, stock draws, and an iterated proportional trade update along existing links | https://github.com/pmarchand1/cereals-network-shocks |
| 3. Food Shock Cascade, PTA and RTA | Heslin et al. 2020, Front. Sustain. Food Syst., doi:10.3389/fsufs.2020.00026 | Published PTA and RTA rules, alpha for usable stocks, kilocalorie scenario library | `main/main.R` modes 0 and 1 |
| 4. Stock-then-split | This repository, flood work | Remaining shortage split between exports and domestic use after stocks are drawn | `main/main.R` mode 2 |

The 2015 module shares no code with the R cascade and does not change PTA, RTA, or stock-then-split. Its rules, data, and differences from the 2015 paper are in [`erl2015/README.md`](erl2015/README.md).

## What a run is

Each country is one node. The time step is one year, long enough for a shock to move through trade. A run starts from a baseline network:

- `P`: production
- `R`: beginning stocks that the scenario is allowed to use
- `E`: bilateral exports. `E_ij` is the flow from country `i` to country `j`. The diagonal is zero.
- `C`: domestic use

Baseline use, before any stock change, is

```
C_i = P_i + imports_i - exports_i
```

Imports are the column sum of `E`. Exports are the row sum.

Supply during the cascade is

```
S_i = P_i + imports_i - exports_i - dR_i
```

`dR_i` is the stock change. A positive `dR` is grain held back, so it reduces supply. A reserve draw makes `dR` smaller and raises supply. Shortage is booked use minus supply:

```
shortage_i = C_i - S_i
```

The code stops if shortage is materially negative. That check is there so the books cannot create grain. The tolerance is `1e-8` times the local scale of use or production, which keeps the same test valid in kilocalories and in tonnes.

## Shocks and parameters

A production shock is a loss. In the scenario files a positive fraction means a decline. Inside the simulator the applied change `dP` is zero or negative, and it cannot exceed baseline production:

```
P_i <- max(P_i + dP_i, 0)
dP_i <= 0
dP_i >= -P_i
```

Alpha is the share of beginning stocks that may be drawn. It is applied once, before the loop, and it is clamped to the interval from 0 to 1:

```
R_i <- alpha * max(R_i, 0)
```

An export ban removes a fraction `f_i` of country `i`'s export row. The withdrawn grain is added to stocks and recorded as a stock build, so the exporter's own supply does not jump when exports fall. Partners lose the imports.

```
q_i = f_i * (sum of row i)
E_ij <- (1 - f_i) * E_ij
R_i <- R_i + q_i
dR_i <- dR_i + q_i
```

The banned country is then frozen, so a later iteration cannot put that grain back on the market.

Each iteration does three things.

1. Draw reserves, up to the shortage and up to usable stocks.

```
draw_i = min(max(shortage_i, 0), R_i)
dR_i <- dR_i - draw_i
R_i <- R_i - draw_i
```

2. Cut domestic use by a small share of the remaining shortage. The share is `gamma = 0.01`. The cut cannot drive use below zero.

```
cut_i = min(gamma * max(shortage_i, 0), max(C_i, 0))
```

3. Reallocate trade. The rule depends on the mode below.

The loop stops when the trade matrix stops changing, or after 50 iterations. At the end, use is set to supply, and exports are cut if supply would otherwise be negative:

```
C_i = max(S_i, 0)
```

World trade cancels. The change in world use equals the production change minus the net stock build, aside from any country whose supply was clipped at zero.

Because `gamma` is 0.01, a production loss is mostly pushed into trade. The shocked country keeps most of its own use and cuts exports. That is the published FSC behavior. It is a different question from "what share of a harvest loss stays in the country that lost the crop?"

## Three modes

`main/main.R` selects the mode with its first argument. PTA is the published default. RTA is the published alternative. Stock-then-split is a third option added for the flood work.

| Argument | Mode | What trade does |
|---|---|---|
| 0 | Proportional trade allocation (PTA) | A country with unmet need cuts its own exports and raises imports along links that already exist. No new link is opened. |
| 1 | Reserves-based trade allocation (RTA) | Exports are cut in proportion to need. Exporters may then offer 20 percent of remaining usable reserves along existing links. |
| 2 | Stock-then-split | Usable stocks are drawn first. A remaining shortage is split between an export cut and a domestic-use cut in proportion to exports and domestic use. Export cuts pass along existing links only. No shipment is raised and no new link is opened. |

PTA. Let `F` be the set of frozen exporters. Country `j` can cover its need from its own exports plus imports from exporters that are not frozen:

```
T_j = exports_j + imports_j from countries not in F
pi_j = min(1, need_j / T_j)
E_ij <- max(0, (1 + 1_{i not in F} * pi_j - pi_i) * E_ij)
```

In the published code, a country is frozen when its usable reserves are zero, and also when a ban has been applied to it. At `alpha = 0` every country has zero usable reserves, so PTA freezes the whole network. Partners of a banned exporter cannot be resupplied by anyone else.

`sim_cascade_PTA` has an extra argument, `freeze_empty_reserves`, default `TRUE`. Set it to `FALSE` to leave stocks unused and still let other exporters shift volume along existing links. Only an explicit ban stays frozen. The flood figures use both settings. The published `main.R` path does not. It always freezes empty reserves.

RTA. After the export cut, an exporter offers

```
offer_i = 0.20 * R_i
```

A banned exporter's offer is set to zero. With `alpha = 0` there is nothing to offer, so RTA leaves the loss on the importers. That is why an RTA run with no stocks looks like PTA with the network frozen.

Stock-then-split (`sim_cascade_split`). After the reserve draw, country `i` with a remaining shortage `s_i` at or above the tolerance cuts

```
export cut_i = s_i * X_i / (X_i + U_i)
use cut_i    = s_i * U_i / (X_i + U_i)
```

where `X_i` is its current exports and `U_i` its current domestic use. The export cut is taken pro rata from its existing export links, so its importers now carry a shortage of their own. The loop repeats until every shortage is below the tolerance. The default tolerance is 1, meaning 1 tonne in the metric-ton flood runs. `main.R` mode 2 works on the kilocalorie scenario library, so it sets the tolerance to one billionth of the largest baseline use instead. Unlike PTA and RTA, this mode does not end by resetting use to supply, so a baseline in which exports already exceed production plus imports is left as it is rather than cut.

Adding mode 2 does not change the PTA or RTA equations, their defaults, or the published outputs. The flood section says which runs use each rule.

## How to run a published scenario

Install the R packages once:

```
Rscript main/Requirements.R
```

The dynamic model takes three arguments: mode (0, 1, or 2), scenario number, alpha.

```
Rscript main/main.R 0 1 0.5
```

That line is PTA, scenario 1 (the one-year wheat shock), and half of beginning stocks available.

```
Rscript main/main.R 1 4 1
```

That line is RTA, scenario 4 (four-year US wheat shock), and all beginning stocks available.

```
Rscript main/main.R 2 1 0
```

That line is stock-then-split, scenario 1, and no beginning stocks available.

The static model only applies the production decline to supply. It does not reallocate trade. It takes the scenario number:

```
Rscript main/main_static.R 1
```

`main.R` and `main_static.R` write fixed file names under `outputs/`. Running them replaces the saved scenario outputs. Use the flood scripts in the next section when the goal is the metric-ton comparison. Those scripts write under `outputs/flood_benchmarks/` and `outputs/export_for_partners/`.

Processed kilocalorie networks for the scenario library already live in `inputs_processed/`. You can skip input preparation for scenarios 1 to 4 and the static scenarios listed below.

To rebuild a processed network, edit the years and the crop lists in `main/ProcessInputs.R`, then:

```
Rscript main/ProcessInputs.R --crop wheat --years 2015:2017 --unit kcal
```

`unit` is `kcal` or `tonnes`. The scenario library was built in kilocalories. Soybean has no kilocalorie factor in the ancillary tables, so a soybean kilocalorie build stops on purpose.

## Input files

### Ancillary lists

| File | Contents |
|---|---|
| `ancillary/country_list195_2012to2016.csv` | The 195 countries: FAO code, ISO3, name |
| `ancillary/cropcommodity_tradelist.csv` | Trade commodities and conversion factors |
| `ancillary/cropcommodity_prodlist.csv` | Production commodities and conversion factors |
| `ancillary/cropcommodity_reserveslist.csv` | Reserve commodities and conversion factors |
| `ancillary/mass_equivalent_factors.csv` | Tonne factors for wheat, maize, and soybean products |

China mainland is FAO 41, ISO3 CHN. Taiwan is FAO 214, ISO3 TWN. They are not combined. Angola is not on the 195-country list.

### Raw data for a new build

Production and trade are FAOSTAT bulk files. Stocks are USDA PSD. Current bulk addresses:

```
https://bulks-faostat.fao.org/production/Production_Crops_Livestock_E_All_Data_(Normalized).zip
https://bulks-faostat.fao.org/production/Trade_DetailedTradeMatrix_E_All_Data_(Normalized).zip
https://apps.fas.usda.gov/psdonline/downloads/psd_alldata_csv.zip
```

The trade zip is served from the production folder on that host. The older `fenixservices.fao.org` links in earlier notes no longer answer.

```
python3 main/fetch_faostat_bulk.py --min-year 2024
```

That script streams the zip to disk, checks that the newest year is at least 2024, and replaces the local production CSV. The trade CSV is large (several gigabytes). The script writes it to the FoodTradeNetwork path used by the exporter. It does not modify any other repository.

### Shock files

A shock file is a table of fractional production declines by country and year. Positive means a decline. Examples:

- `inputs/Scenario1_COVID_WheatDeclineFraction_1Year_195countries.csv`
- `inputs/Scenario4_USDustBowl_WheatDeclineFraction_4Years_195countries.csv`
- `inputs/placeholder_flood_shocks.csv` for the flood comparison

### Processed files

`ProcessInputs.R` writes `inputs_processed/`: an export matrix `E0`, production `P0`, and reserves `R0`, plus the shock file copied forward. The published scenarios are in kilocalories on a 2015 to 2017 mean network.

## Flood scenarios

### Data that stays local

The draft flood-shock table and everything built from it in `outputs/flood_shocks/` are kept on the author's machine and are not distributed. Both are listed in `.gitignore`. The draft rows are provisional and will change as the Sentinel-1 archive is processed. The metric-ton network package, the placeholder shocks, and the benchmark outputs described below are tracked and can be shared.

This section describes metric-ton flood scenarios for rice, wheat, and maize over 2002 to 2024. The comparison on the 2018 to 2021 mean still uses illustrative placeholder fractions. The observed draft shock table is a separate run, on each year's own network.

### What was run, and what was not

The flood figures on the 2018 to 2021 mean use the FSC rules listed below. The observed draft table is run separately, with stock-then-split as the primary rule and modified PTA as the sensitivity.

| Run | Rule in the code | Stocks | Trade |
|---|---|---|---|
| PTA, frozen | `freeze_empty_reserves = TRUE` | Unused when alpha is 0 | Every exporter with zero usable reserves is frozen. At alpha 0 that is everyone. |
| PTA, reroute | `freeze_empty_reserves = FALSE` | Unused when alpha is 0 | Other exporters can still move grain on existing links. A ban stays closed. |
| RTA | `sim_cascade_RTA` | Same alpha | Extra trade is offered only out of remaining reserves. At alpha 0 there is no offer. |

Alpha 1 in those runs means all USDA beginning stocks may be drawn. The published default remains PTA with the freeze on. Turning the freeze off is a comparison switch for the flood figures. It is not a new default.

The placeholder shocks are illustrative, not estimates of historical flood losses. Each row is marked `illustrative_placeholder`. A US shock affects the whole United States node, not just a Midwest subset. The year column is metadata. These runs stay on the 2018 to 2021 mean even when the year is earlier. Several rows with the same `event_id` are one scenario, so more than one country can lose production together.

| Event | Crop | Node | Production loss | Reactive ban |
|---|---|---|---|---|
| Thailand 2011 | Milled rice | THA | 15% | 95% |
| Pakistan 2022 | Wheat | PAK | 10% | 95% |
| Pakistan 2022 | Milled rice | PAK | 10% | 95% |
| Myanmar 2008 | Milled rice | MMR | 15% | 95% |
| US Midwest 2011 | Maize | USA | 10% | 95% |
| US Midwest 2011 | Wheat | USA | 5% | 95% |

Replace `inputs/placeholder_flood_shocks.csv` with validated flood-loss estimates and rerun:

```
Rscript main/run_allocation_comparison.R
```

Each row needs `event_id`, `crop`, `year`, `iso3` or `fao_code`, and either `production_loss_fraction` or `production_loss_mt`. A fraction is a positive share of baseline production. Tonnes are positive losses. `reactive_ban_fraction` is optional and is the sensitivity, not the flood experiment. `shock_source` records provenance. `main/read_flood_shocks.R` rejects an unknown country, a duplicate country inside one event, a fraction outside (0, 1], and a tonne loss above that country's baseline production. Do not treat the current maps as historical results. Do not switch these benchmarks onto the annual networks unless that is an explicit later decision. The annual files remain in the network package.

The observed draft flood-shock table is an Excel workbook with a `shocks` sheet. It is kept local and is not part of this repository. `main/run_flood_shocks.R` reads the path in `FSC_FLOOD_TABLE`, or else the newest file in `data/` that matches `*flood_shock_table*.xlsx`. One row is one crop, country, and calendar year. The applied loss is `shock_total_t`, which already adds the same-year loss and the second-year loss from floods in the previous year. Rice is milled. Three crop-country-years are capped at production: Papua New Guinea 2003 rice and maize, and Turkmenistan 2016 maize. Rows for 2015 to 2024 are a sample of the Sentinel-1 archive and are expected to change. Rows for 2002 to 2014 are not.

```
Rscript main/run_flood_shocks.R
```

That script builds one network for each crop and year from the annual package. It uses reported beginning stocks and treats a blank as zero. It does not use the imputed stock fill, a model price, or an export ban. Alpha is only 0 or 1. Every shocked country in the same crop-year is applied together. Results go to `outputs/flood_shocks/`, which is also kept local. The lead question is whether the consumption loss stays in the directly shocked countries. That is a result to read from the run, not a premise.

```
python3 main/plot_flood_report.py
```

That writes `outputs/flood_shocks/REPORT.md`, a Word copy, and the maps, stock-to-use charts, and trade networks. The 2015 to 2024 rows in the draft table are provisional.

### Network package

`main/export_inputs.py` writes `outputs/export_for_partners/` and `outputs/export_for_partners.zip`. Units are metric tons. Years are 2002 to 2024. Crops are milled rice, wheat, maize, and soybean. `README_FSC_Inputs.md` in that folder is the column-by-column note.

Rice production is FAO item 27 (paddy) times 0.67. Rice trade uses 0.67 for paddy, 0.8375 for husked rice, and 1 for milled rice, broken rice, and rice flour. PSD `Rice, Milled` is already milled and is not scaled again. Wheat trade adds flour, pasta, bread, bulgur, germ, and pastry as wheat-grain equivalents. Maize trade adds germ and flour. Maize oil and green maize are excluded. Soybean is beans only. Oil and cake are excluded. The factors are in `ancillary/mass_equivalent_factors.csv`. They are project assumptions. PSD values are published in thousand metric tons and stored here in metric tons. The PSD market year is joined to the FAO calendar year on the year number. There is no marketing-year shift.

Trade is FAOSTAT export quantity. Self-trade is dropped. Flows outside the 195-country list are dropped. Re-exports are netted for Hong Kong, the United Arab Emirates, Singapore, the Netherlands, and Italy. For each hub, re-exports are `min(exports, imports)`, and the export row is scaled down by that amount. The removed flow is not assigned to an upstream seller. `trade_mt_net` is the column to use. Its world sum is a sum of bilateral flows, so it can sit above a published "world trade" total.

`beginning_stocks_mt` is blank when USDA has no row. `PSD_Missing_Flag` is 1 only then. `reserves_imputed_mt` is optional: the same-year median stock-to-use of countries with a reported stock, times domestic use. The flood benchmark fills missing stocks with this imputation, then with zero if needed. Both columns are in the file.

EU stocks are one USDA row and are shared across the EU-28 member list in `ProcessInputs_funcs.R`, including the United Kingdom. Malta is in the 195 and is not in that member list.

Rebuild:

```
python3 main/export_inputs.py
```

Or pass new FAOSTAT paths after a bulk refresh:

```
python3 main/export_inputs.py \
  --production /path/to/Production_Crops_Livestock_E_All_Data_(Normalized).csv \
  --trade /path/to/Trade_DetailedTradeMatrix_E_All_Data_(Normalized).csv \
  --psd /path/to/psd_alldata.csv
```

`main/export_inputs.R` is a thin wrapper around the same Python script. Run it from the repository root.

### Benchmark and figures

The benchmark builds 2018 to 2021 mean networks from the metric-ton network package and runs export bans plus the placeholder flood shocks.

```
Rscript main/run_flood_benchmarks.R
```

Bans cover wheat exporters (Russia, Canada, EU-28, the United States, Ukraine, Australia, Argentina, Kazakhstan, Pakistan) and rice exporters (India, Thailand, Vietnam, Pakistan, the United States, Myanmar). Restrictions are 50% and 95%. Durations are a full year and six months, with the six-month case counted as half of the annual fraction. Alpha is 0, 0.25, 0.5, and 1. Results go to `outputs/flood_benchmarks/benchmark_results.csv`. The rice price column in that file uses an uncalibrated index (`beta_surplus = -1`, `beta_stu = -0.5`). It is not a world-price forecast and it is not plotted.

Country and edge detail for the figures:

```
Rscript main/export_figure_cases.R
python3 main/plot_report_figures.py
```

Figures and `REPORT.md` land in `outputs/flood_benchmarks/`. The figure cases are full-year 95% bans at alpha 0 and alpha 1, for the frozen PTA rule, the reroute PTA rule, and RTA on the headline shocks. The longer model note, with the food-security framing, is `outputs/flood_benchmarks/FSC_model_food_security.docx`.

Headline availability results on the 2018 to 2021 mean, 95% bans:

| Shock | Stocks unused, trade frozen | Stocks unused, trade can reroute | Beginning stocks usable |
|---|---|---|---|
| Russia wheat | 33.85 Mt (4.44% of world use) | 33.85 Mt (4.44%) | 0.18 Mt (0.02%) |
| India rice | 13.41 Mt (2.60%) | 13.41 Mt (2.60%) | 0.22 Mt (0.04%) |
| US maize | 55.86 Mt (4.86%) | 55.86 Mt (4.86%) | 4.02 Mt (0.35%) |

The world total does not change when trade is allowed to reroute. The world total is the grain held back as stocks. Rerouting changes which countries eat less. With trade frozen, an Indian rice ban cuts Gulf use by about 70%. With rerouting, that Gulf loss is about 15%, and the tonnes show up as lower use in other rice exporters.

These percentages are shares of that commodity's use. They are not shares of calories or of a household diet.

### Which allocation rule for a flood run

`main/run_allocation_comparison.R` runs the same 2018 to 2021 mean network under three existing settings. It does not add a new trade rule.

| Name in the output | Code | Role |
|---|---|---|
| `modified_pta` | `sim_cascade_PTA`, `freeze_empty_reserves = FALSE` | Preferred candidate for a flood run, not an empirically settled winner. Other exporters can still move grain on existing links when usable reserves are zero. An explicit ban stays closed. The proportional update can multiply a thin existing link. |
| `published_pta` | `sim_cascade_PTA`, `freeze_empty_reserves = TRUE` | The published reference. At alpha 0 every exporter is frozen. `main.R` still uses this. |
| `rta` | `sim_cascade_RTA`, `rexp = 0.2` | Reserves-offer sensitivity. Extra exports come only from remaining reserves, and only one fifth of those reserves is offered on each pass. At alpha 0 that offer is zero, so the result matches published PTA. |

The grid is every placeholder flood, four values of alpha (0, 0.25, 0.5, 1), and two policies: the production shock alone, and that shock plus the reactive ban in the shock file. The production shock alone is the flood experiment. The ban is a sensitivity. The same script also repeats the wheat and rice export-ban grid under all three rules. `benchmark_results.csv` is left as the published PTA record.

On the placeholders, with reserves closed, the world consumption loss is the same under all three rules. The shocked country's share of that loss is not. Modified PTA keeps that share between 1.8 and 9.9 percent. Published PTA keeps it near 1 percent for a large exporter and much higher for a smaller one. Domestic use still takes only 1 percent of the remaining shortage on each pass, so the loss still leaves mostly through trade. That step was not changed.

Modified PTA often reaches the 50-iteration cap when alpha is 0. The world total is stable if the same case is allowed to run longer. Country totals can still shift, so a partner ranking from a capped run is approximate. The cap was not raised. Mass-balance notes, including the constant maize residual and the EU-28 wheat gap under the published rule, are in `outputs/flood_benchmarks/ALLOCATION_COMPARISON.md`.

```
Rscript main/run_allocation_comparison.R
```

### Checks on the flood package

On the files as built: production has 195 countries in every year from 2002 to 2024, no duplicate country-years, and no negative tonnes. Rice production uses the single factor 0.67. Trade files have no self-trade and no negative flows. 2024 production in the package is 798 Mt of wheat, 1,215 Mt of maize, 398 Mt of soybean, and 549 Mt of milled rice. About a quarter of wheat stock rows have no USDA beginning stock. They are flagged, not filled with a silent zero.

Wheat and rice ban runs close the world balance within 1 kt, except the EU-28 wheat ban with trade frozen, where the gap is about 0.37 Mt. Every maize run carries the same gap of about 0.12 Mt. That equals baseline maize use below zero in Slovenia and Luxembourg: their processed-product exports exceed production plus imports. The diagnostic floors that negative use at zero. The cascade does not create the gap, and the outputs are not adjusted to hide it. Rice has the same kind of gap, about 0.65 kt, in Slovenia. Modified PTA often hits the 50-iteration cap when alpha is 0. The world total is stable if the loop continues, and some country totals still move, so a partner ranking from a capped run is approximate.

```
Rscript tests/test_fsc_safety.R
Rscript tests/test_allocation_specs.R
Rscript tests/test_rice_price_step.R
Rscript tests/test_process_inputs.R
```

The safety test checks non-negative production, use, and exports, a zero diagonal, the alpha clamp, and the export-ban accounting. It does not reproduce a published table from the 2020 paper.

## Outputs of a published run

`main.R` writes three groups of CSV files under `outputs/`. Units follow `ProcessInputs.R`, usually kilocalories.

Bilateral exports, origin by destination, with one block of columns per year:

- `BilateralExportMatrix_TimeSeries.csv`

Country time series (`iso3`, country name, year, value):

- `Production_TimeSeries.csv`
- `Reserve_TimeSeries.csv`
- `Shortage_TimeSeries.csv`
- `ConsumptiontoC0_TimeSeries.csv` (use divided by initial use)
- `ReserveChangetoC0_TimeSeries.csv` (stock change divided by initial use)

Network summaries at the start and the end of the run:

- `Export_InitialTotalByCountry.csv`, `Export_FinalTotalByCountry.csv`
- `Import_InitialTotalByCountry.csv`, `Import_FinalTotalByCountry.csv`
- `NumberExportTradePartners_InitialTotalByCountry.csv`, `NumberExportTradePartners_FinalTotalByCountry.csv`
- `NumberImportTradePartners_InitialTotalByCountry.csv`, `NumberImportTradePartners_FinalTotalByCountry.csv`

## Scripts

| Script | Role |
|---|---|
| `main/main.R` | Dynamic FSC. Argument 1 is PTA (0), RTA (1), or stock-then-split (2). Argument 2 is the scenario. Argument 3 is alpha. Writes `outputs/`. |
| `main/main_static.R` | Production shock only. One argument, the scenario number. Writes `outputs/`. |
| `main/ProcessInputs.R` | Builds `E0`, `P0`, `R0` from FAOSTAT and PSD. |
| `main/FSC_sim_funcs.R` | `sim_cascade_PTA`, `sim_cascade_RTA`, and `sim_cascade_split`. |
| `main/FSC_component_funcs.R` | Supply, shortage, reserve draw, trade update, export bans. |
| `main/fetch_faostat_bulk.py` | Downloads FAOSTAT production and trade through at least 2024. |
| `main/export_inputs.py` | Writes the metric-ton package for 2002 to 2024. |
| `main/run_flood_benchmarks.R` | Export-ban and placeholder-flood grid. Writes `outputs/flood_benchmarks/`. |
| `main/run_allocation_comparison.R` | Same baseline under modified PTA, published PTA, and RTA. |
| `main/run_flood_shocks.R` | Draft flood table on each year's network. Stock-then-split and modified PTA. |
| `main/plot_flood_report.py` | Maps, stock-to-use, trade networks, and the flood report. |
| `main/export_figure_cases.R` | Country and edge results for the figures. |
| `main/plot_report_figures.py` | Maps and charts, and `outputs/flood_benchmarks/REPORT.md`. |
| `R/rice_price_step.R` | Uncalibrated rice price index. Not used in the figures. |

## Scenario library

Scenarios 1 to 4 are the dynamic runs in `main.R`. The static script uses the same shock files for scenarios 1 to 3, and a separate block of wheat, maize, and rice cases from 11 upward. Baselines for this library are 2015 to 2017 means, in kilocalories. Production fractions below are declines.

### Dynamic scenarios

**1. Wheat, one year.** COVID-19 and locust disruption, 2020. Kenya 0.4916, Saudi Arabia 0.4483, Yemen 0.2335, Ukraine 0.205, Kazakhstan 0.1818, Iran 0.1024, Pakistan 0.0976, Ethiopia 0.0874, Russia 0.0768.

**2. Maize, one year.** Same class of shock. Saudi Arabia 0.5281, Somalia 0.4844, Iran 0.2975, Yemen 0.2518, Kenya 0.1521, Brazil 0.1328, Ethiopia 0.1301, Argentina 0.119, Ukraine 0.0878, Pakistan 0.0717.

**3. Rice, one year.** Kenya 0.3425, Pakistan 0.197, Iran 0.1883, Thailand 0.021, India 0.0192, Viet Nam 0.0012.

**4. US wheat, four years.** Dust Bowl analogue. US decline fractions 0.33, 0.36, 0.24, 0.24.

### Static scenarios

Scenarios 1 to 3 repeat the shocks above without trade reallocation.

Scenarios 11 to 16 are wheat on the 2015 to 2017 network. Scenarios 21 to 26 are maize. Scenarios 31 to 34 are rice. Input names:

| Number | Run name | Shock or restriction file |
|---|---|---|
| 11 | Wheat_breadbasket_Year2008 | `Scenario_Wheat_DeclineFraction_breadbasket_Year2008_195countries.csv` |
| 12 | Wheat_locust_Year2020 | `Scenario_Wheat_DeclineFraction_locust_Year2020_195countries.csv` |
| 13 | Wheat_locust_Year2020_breadbasket_Year2008 | `Scenario_Wheat_DeclineFraction_locust_Year2020_breadbasket_year2008_195countries.csv` |
| 14 | Wheat_ExportRestrictionFraction_Year2008 | `Scenario_Wheat_ExportRestrictionFraction_Year2008_195countries.csv` |
| 15 | Wheat_ExportRestrictionFraction_Year2008with_RUS_25 | `Scenario_Wheat_ExportRestrictionFraction_Year2008_with_RUS_25%_195countries.csv` |
| 16 | Wheat_ExportRestrictionFraction_Year2008with_RUS_50 | `Scenario_Wheat_ExportRestrictionFraction_Year2008_with_RUS_50%_195countries.csv` |
| 21 | Maize_breadbasket_Year2008 | `Scenario_Maize_DeclineFraction_breadbasket_Year2008_195countries.csv` |
| 22 | Maize_locust_Year2020 | `Scenario_Maize_DeclineFraction_locust_Year2020_195countries.csv` |
| 23 | Maize_locust_Year2020_breadbasket_Year2008 | `Scenario_Maize_DeclineFraction_locust_Year2020_breadbasket_year2008_195countries.csv` |
| 24 | Maize_ExportRestrictionFraction_Year2008 | `Scenario_Maize_ExportRestrictionFraction_Year2008_195countries.csv` |
| 25 | Maize_ExportRestrictionFraction_Year2008with_ARG_25 | `Scenario_Maize_ExportRestrictionFraction_Year2008_with_ARG_25%_195countries.csv` |
| 26 | Maize_ExportRestrictionFraction_Year2008with_ARG_50 | `Scenario_Maize_ExportRestrictionFraction_Year2008_with_ARG_50%_195countries.csv` |
| 31 | Rice_breadbasket_Year2008 | `Scenario_Rice_DeclineFraction_breadbasket_Year2008_195countries.csv` |
| 32 | Rice_locust_Year2020 | `Scenario_Rice_DeclineFraction_locust_Year2020_195countries.csv` |
| 33 | Rice_locust_Year2020_breadbasket_Year2008 | `Scenario_Rice_DeclineFraction_locust_Year2020_breadbasket_year2008_195countries.csv` |
| 34 | Rice_ExportRestrictionFraction_Year2008 | `Scenario_Rice_ExportRestrictionFraction_Year2008_195countries.csv` |

Those CSV paths sit under `inputs/2021_C2P2_scenarios/`. Scenarios 14 to 16, 24 to 26, and 34 are export restrictions with no production decline. The others are production declines with no export restriction.

## Runtime

Building inputs from the raw FAOSTAT files takes longer than a cascade. A cascade on the 195-country network is a few minutes on a desktop. The metric-ton benchmark grid is shorter than that once the network CSVs exist.

## How to cite

If you use this model, please cite the papers it comes from.

- Puma, M. J., Bose, S., Chon, S. Y., and Cook, B. I. (2015). Assessing the evolving fragility of the global food system. *Environmental Research Letters*, 10, 024007. doi:[10.1088/1748-9326/10/2/024007](https://doi.org/10.1088/1748-9326/10/2/024007)
- Marchand, P., et al. (2016). Reserves and trade jointly determine exposure to food supply shocks. *Environmental Research Letters*, 11, 095009. doi:[10.1088/1748-9326/11/9/095009](https://doi.org/10.1088/1748-9326/11/9/095009)
- Heslin, A., Puma, M. J., Marchand, P., et al. (2020). Simulating the cascading effects of an extreme agricultural production shock. *Frontiers in Sustainable Food Systems*, 4, 26. doi:[10.3389/fsufs.2020.00026](https://doi.org/10.3389/fsufs.2020.00026)

`CITATION.cff` in the repository root holds the same references in machine-readable form, and GitHub shows it under "Cite this repository".

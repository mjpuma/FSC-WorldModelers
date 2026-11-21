# Puma et al. (2015), Python implementation updated for recent data

This folder is a Python implementation of the model in Puma, M.J., Bose, S., Chon, S.Y., and Cook, B.I. (2015), *Assessing the evolving fragility of the global food system*, Environmental Research Letters 10, 024007, doi:10.1088/1748-9326/10/2/024007. It is updated for FAOSTAT data through 2024. It is the first step in the Food Shock Cascade line; the [repository README](../README.md#lineage) traces the steps after it.

It is a new implementation, not a port of the 2015 MATLAB code (which is kept unchanged in [`original/`](original/)), and it is not built to reproduce the paper's numbers exactly. The rules are the paper's. The data, the units, and a few choices listed under [Differences from the 2015 paper](#differences-from-the-2015-paper) are current.

## The model

Each crop-year is a directed, weighted network. Nodes are countries. `W[i, j]` is the flow of the crop from exporter `i` to importer `j`, in tonnes. Wheat is in wheat-grain equivalent and rice in milled-rice equivalent.

A disturbance is an export ban on a set of countries. There is no production loss and no cascade.

1. **Ban.** A banned country `i` with ban fraction `f_i` keeps `(1 - f_i)` of every export link. It still imports. The withheld volume stays in its domestic supply.
2. **Static accounting.** No reallocation. Importer `j` loses the imports it received from banned exporters: `loss_j = s_in_j - s_in_after_j`.
3. **Dynamic accounting.** The volume still traded after the ban, `P = sum(W_after)`, is one world pool. Importers are served in descending GDP order (constant 2015 US dollars). Each receives up to its full baseline imports `s_in_j` until the pool runs out; the next importer receives the remainder, and later importers receive nothing. Links are ignored. This is the paper's end-member case in which wealthier countries outbid poorer ones.
4. **Loss share.** A country's loss is divided by its staple food supply: FAOSTAT domestic supply of cereals excluding beer plus 0.26 times starchy roots, in cereal-equivalent tonnes.

Every run checks its own tonnage. The trade removed by the ban equals the sum of static losses, equals the sum of dynamic losses, and equals the volume withheld by banned exporters, year by year. A run that fails a check stops with `BalanceError`.

Network metrics follow section 2.1 of the paper: degree `k` and strength `s` for exports and imports, links `L`, global trade `G_total`, active-node averages and maxima, a least-squares power law `s_out ∝ k_out^alpha`, and betweenness on the undirected, unweighted network with the standard normalization `2 / ((N - 1)(N - 2))`. Self-sufficiency is cereal-equivalent production over staple supply.

## Run it

From this folder (`erl2015/`):

```bash
pip install -r requirements.txt
```

```bash
python3 -m fsc2015 prepare
```

`prepare` streams the FAOSTAT detailed trade matrix already cached at `inputs/faostat_bulk_cache/trade.zip` (fetch it with `main/fetch_faostat_bulk.py` if it is missing) and keeps the wheat and rice quantity rows. It downloads the FAOSTAT food balance sheets (current and historic) and Macro Indicators. Extracts go to `erl2015/data/`, which is gitignored. The trade step takes a few minutes; the rest take seconds.

```bash
python3 -m fsc2015 check
```

```bash
python3 -m fsc2015 run
```

`run` runs both paper scenarios for 1992–1996 and 2019–2023 and writes `outputs/erl2015/`. Options:

```bash
python3 -m fsc2015 run --crop wheat --scenario europe_wheat_1816 --period 2005-2009 --period 2019-2023
```

`--mirror` picks how exporter and importer reports are combined (`mean`, the default, `max`, `export`, or `import`). `--min-flow` sets the smallest flow kept, in tonnes (default 1). A scenario is any CSV with columns `m49`, `name`, and optional `ban_fraction` in (0, 1].

```bash
python3 -m pytest -q tests
```

The unit tests use small made-up networks with hand-worked answers. Three integration tests run when the extracts exist and check tonnage closure and units on the real data.

## Paper figures

```bash
python3 -m fsc2015 figures
```

This redraws figures 2 to 5 of the paper into `outputs/erl2015/`. Add `--skip-paleo` to leave out figure 3, or `--period` (repeatable) to change the windows for figures 4 and 5 (default 1992–1996, 2005–2009, 2019–2023; figure 5 maps the first and last).

| File | Paper figure | Data |
|---|---|---|
| `fig2_exporters_2004_2006.png` | 2. Exporters with 90% of world exports, 2004–2006 | FAOSTAT trade. Asterisks for 2007–2008 export restrictions come from `ancillary/export_restrictions_2008.csv`, which is empty until a sourced list is added |
| `fig3_disturbances.png` | 3. 1816 summer temperature anomaly and 1876–1878 summer PDSI | Luterbacher et al. (2004) European seasonal temperature reconstruction, 0.5 degree, anomaly against 1971–2000; Cook et al. (2010) Monsoon Asia Drought Atlas. Both downloaded from NOAA NCEI Paleoclimatology on first use |
| `fig4_strength_degree.png`, `fig4_power_law.csv` | 4. Export strength vs export degree with the power-law fit and the component-wise median | FAOSTAT trade; nodes that exported in at least one year of the period |
| `fig5_ssr_maps.png`, `fig5_ssr.csv` | 5. Self-sufficiency by country in two periods and the change | FAOSTAT food balances; Natural Earth 1:50m outlines, downloaded on first use |

The power-law exponents come out at 1.78 (1992–1996), 1.91 (2005–2009) and 2.03 (2019–2023) for wheat, and 1.78, 1.81 and 1.91 for rice. The paper reported 1.78 to 1.92 for its two periods.

## Inputs

| Input | Source | Notes |
|---|---|---|
| Bilateral trade, 1986–2024 | FAOSTAT Detailed Trade Matrix, bulk download | Wheat items 15, 16, 18, 19, 20, 21, 22; rice items 27, 28, 29, 31, 32, 38. Factors as in `ancillary/mass_equivalent_factors.csv` at the repository root (flour and pasta 1/0.78, bulgur 1/0.90, bread 0.9; paddy 0.67, husked 0.8375) |
| Staple supply, 1961–2023 | FAOSTAT Food Balances, current (2010 on) and historic (to 2013) | Items 2905 and 2907, element 5301. The current series is used where both exist |
| GDP, 1970–2025 | FAOSTAT Macro Indicators | `Value US$, 2015 prices`. Importers with no GDP are served last, largest first |
| Least developed countries | `ancillary/un_ldc_list.csv` | The 44 UN LDCs after São Tomé and Príncipe graduated in December 2024, plus Ethiopia PDR and Sudan (former) for early years |
| LDC summary exclusions | `ancillary/ldc_summary_exclusions.csv` | Djibouti, a transit port for Ethiopia. Any LDC whose imports of the crop exceed its whole staple supply is also flagged |
| Ban scenarios | `scenarios/*.csv` | Countries starred in table S1 of the paper, plus Belgium-Luxembourg and Czechoslovakia for early years |

Exporter and importer reports of the same flow are combined by their mean when both exist, and the single report otherwise. China is the mainland (FAO area 41); Hong Kong, Macao, and Taiwan are separate nodes, and the FAO "China" aggregate is dropped.

From 2014, FAOSTAT reports flows below one tonne that earlier years round away. Left in, they add about 1,200 wheat links in 2014 alone. Flows under 1 tonne are dropped so link counts are comparable across years. 2024 is the most recent trade year and is incomplete, so the default late window ends in 2023, the last year of the food balances.

## Outputs

`outputs/erl2015/REPORT.md` collects the tables and figures below. Each scenario folder holds the CSVs.

| File | Contents |
|---|---|
| `table1_network.csv` | Network metrics by period (paper table 1) |
| `table2_disturbance.csv` | Loss in each metric from the ban, absolute and percent (paper table 2) |
| `ldc_summary.csv` | Median and mean LDC loss shares, static and dynamic (paper figure 6) |
| `countries_by_period.csv` | Every country's imports, losses, staple supply, loss shares, LDC and transit flags |
| `top_exporters.csv`, `top_betweenness.csv` | Top 10 in the last year of each period (paper tables S3, S4) |
| `power_law.csv`, `period_nodes.csv` | Strength-degree fit and the per-node means behind it (paper figure 4) |
| `yearly_before.csv`, `yearly_after.csv`, `bans.csv` | Yearly metrics and lost trade |
| `timeseries_network.csv`, `fig_timeseries.png` | Links and trade, 1986–2024 (paper figure S2) |
| `ssr_by_country.csv` | Self-sufficiency by country and period (paper figure 5) |

## How it compares with the 2015 paper

The 1992–1996 window overlaps the paper's first period, so it is the fair comparison. Rice in the paper was in paddy tonnes; dividing by 1.49 gives a rough milled figure.

| Quantity, 1992–1996 | Paper | This implementation |
|---|---|---|
| Wheat trade, Mt/yr | 116 | 108 |
| Rice trade, Mt/yr | 24.6 paddy (about 16.5 milled) | 16.0 milled |
| Wheat trade lost to the European ban | 38 Mt (33%) | 35.8 Mt (33%) |
| Rice trade lost to the Asian ban | 15 Mt paddy (59%) | 9.2 Mt milled (58%) |
| LDC wheat loss, dynamic, median / mean | 3.8% / 8.9% | 3.3% / 8.2% |
| LDC rice loss, dynamic, median / mean | 2.2% / 8.2% | 1.8% / 4.9% |
| Largest LDC wheat losses, dynamic | Yemen 55%, Mauritania 40%, Kiribati 32%, Haiti 24% | Yemen 56%, Mauritania 41%, Kiribati 32%, Haiti 23% |
| Largest LDC rice losses, dynamic | Comoros 45%, Senegal 31%, Haiti 23%, Benin 23%, Togo 22% | Senegal 21%, Togo 16%, Haiti 16%, Benin 15% |
| Power-law exponent alpha | 1.78 to 1.92 | 1.78 (both crops) |
| Countries with low or marginal SSR | about 85% | 87% |

The rice loss shares are lower here mainly because losses are now milled tonnes over a milled-basis staple supply. Trade volumes differ because FAOSTAT has revised 1990s trade since 2012 and because breakfast cereals are no longer counted as wheat.

## Differences from the 2015 paper

| Topic | 2015 paper | Here |
|---|---|---|
| Language and data | MATLAB, FAOSTAT downloaded 2012–2013, to 2009 | Python, FAOSTAT bulk files, to 2024 |
| Rice unit | Paddy equivalent, divided by a milled-basis supply | Milled equivalent throughout |
| Wheat items | Included breakfast cereals | Excludes breakfast cereals (mixed grains); includes wheat germ |
| Conversion factors | FAO commodity trees | The repository's FSC factors |
| Small flows | All reported flows | Flows under 1 t dropped |
| Betweenness | Normalization printed in eq. S1 | Standard `2 / ((N - 1)(N - 2))` |
| Staple supply | Production + imports - exports + stock change, computed | FAOSTAT domestic supply quantity, which is the same identity |
| GDP | UN National Accounts, 2005 US dollars | FAOSTAT Macro Indicators, 2015 US dollars |
| LDC list | FAOSTAT, February 2014, 48 countries | UN list, 44 countries |
| Late period | 2005–2009 | 2019–2023 by default; any window can be passed |

## Package layout

| Module | Role |
|---|---|
| `fsc2015/config.py` | Items, factors, FAOSTAT codes, paths, default periods |
| `fsc2015/data.py` | Downloads, extracts, and validated loaders |
| `fsc2015/network.py` | Unit conversion, mirror rule, `TradeNetwork` |
| `fsc2015/metrics.py` | Degree, strength, betweenness, power law, staple supply, SSR |
| `fsc2015/disturbance.py` | Ban, static and dynamic accounting, balance checks |
| `fsc2015/pipeline.py` | Runs a scenario over periods and writes CSVs |
| `fsc2015/report.py` | Run figures and `REPORT.md` |
| `fsc2015/figures.py` | Paper figures 2 to 5 |
| `fsc2015/__main__.py` | Command line |

The original MATLAB scripts behind the 2015 paper are in [`original/`](original/), unchanged, as a record of what produced the published results. See [`original/README.md`](original/README.md).

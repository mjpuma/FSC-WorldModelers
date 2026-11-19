# Allocation comparison on the 2018-2021 mean network

Three existing FSC settings. No new allocation rule was added. The flood rows use the placeholder fractions in `inputs/yawei_placeholder_flood_shocks.csv`. Those fractions are not observed flood losses.

Modified PTA is `sim_cascade_PTA` with `freeze_empty_reserves = FALSE`. Published PTA is the same function with that argument `TRUE`, which is what `main.R` runs. RTA is `sim_cascade_RTA` with its existing `rexp = 0.2`. A banned exporter stays frozen in all three. Every call passes the rule explicitly.

Flood runs: 144, all completed. Converged before 50 iterations: 124 of 144. Export-ban runs: 720, converged: 656 of 720. New bilateral links: 0. Negative production, reserves, trade, or final use: 0. Reserve draws above usable reserves: 0. Production change off the requested shock by more than 1 gram: 0.

Mass balance is the change in use, minus the change in production, plus the stock change. Maize carries one constant gap of -0.120 Mt on every maize run, so that residual is in the baseline maize balance. Rice sits between -0.651 and -0.466 kt. Wheat gaps larger than 1 kt: wheat_EU-28_ban95_full, published_pta, alpha 0, +0.365 Mt; wheat_EU-28_ban95_full, rta, alpha 0, +0.365 Mt; wheat_EU-28_ban95_full, published_pta, alpha 0.25, +0.034 Mt.

Modified PTA at alpha 0 hit the 50-iteration cap on 12 of 12 flood runs. Published PTA converged on 24 of 24 production-only flood runs. RTA converged on 23 of 24. The same equations were then run to 200 iterations for two alpha-0 cases. Thailand rice world loss was 3.20 Mt at 50 iterations and 3.20 Mt at 200, and Thailand's share moved from 6.21 to 6.23 percent. US maize world loss was 36.37 Mt at 50 iterations and 36.37 Mt at 200. The largest country-level maize change between those two caps was 75.9 kt. Partner rankings from a run that hit the cap are approximate. The iteration limit in the grid was not raised.

World consumption loss, production shock only, million tonnes. This is the primary flood experiment.

| Event | Alpha | Modified PTA | Published PTA | RTA |
|---|---:|---:|---:|---:|
| thailand_2011 | 0 | 3.20 | 3.20 | 3.20 |
| thailand_2011 | 0.25 | 0.04 | 0.04 | 0.03 |
| thailand_2011 | 0.5 | 0.02 | 0.02 | 0.02 |
| thailand_2011 | 1 | 0.00 | 0.00 | 0.00 |
| pakistan_2022_wheat | 0 | 2.55 | 2.55 | 2.55 |
| pakistan_2022_wheat | 0.25 | 0.02 | 0.02 | 0.03 |
| pakistan_2022_wheat | 0.5 | 0.01 | 0.01 | 0.02 |
| pakistan_2022_wheat | 1 | 0.00 | 0.00 | 0.00 |
| pakistan_2022_rice | 0 | 0.81 | 0.81 | 0.81 |
| pakistan_2022_rice | 0.25 | 0.01 | 0.01 | 0.01 |
| pakistan_2022_rice | 0.5 | 0.00 | 0.00 | 0.00 |
| pakistan_2022_rice | 1 | 0.00 | 0.00 | 0.00 |
| myanmar_2008 | 0 | 2.73 | 2.73 | 2.73 |
| myanmar_2008 | 0.25 | 0.07 | 0.07 | 0.04 |
| myanmar_2008 | 0.5 | 0.05 | 0.05 | 0.03 |
| myanmar_2008 | 1 | 0.03 | 0.02 | 0.02 |
| us_midwest_2011_maize | 0 | 36.37 | 36.37 | 36.37 |
| us_midwest_2011_maize | 0.25 | 4.06 | 4.50 | 0.97 |
| us_midwest_2011_maize | 0.5 | 0.41 | 0.29 | 0.28 |
| us_midwest_2011_maize | 1 | 0.12 | 0.12 | 0.12 |
| us_midwest_2011_wheat | 0 | 2.48 | 2.48 | 2.48 |
| us_midwest_2011_wheat | 0.25 | 0.00 | 0.00 | 0.00 |
| us_midwest_2011_wheat | 0.5 | 0.00 | 0.00 | 0.00 |
| us_midwest_2011_wheat | 1 | 0.00 | 0.00 | 0.00 |

Share of that consumption loss borne by the shocked country, production shock only, alpha 0, percent.

| Event | Modified PTA | Published PTA | RTA |
|---|---:|---:|---:|
| thailand_2011 | 6.2 | 1.0 | 1.0 |
| pakistan_2022_wheat | 1.8 | 67.1 | 67.1 |
| pakistan_2022_rice | 4.9 | 1.0 | 1.0 |
| myanmar_2008 | 4.1 | 37.1 | 37.1 |
| us_midwest_2011_maize | 9.9 | 1.0 | 1.0 |
| us_midwest_2011_wheat | 7.9 | 1.2 | 1.4 |

World consumption loss when the placeholder production shock is combined with the file's reactive export restriction. This is a sensitivity, not the primary flood experiment. Million tonnes.

| Event | Alpha | Modified PTA | Published PTA | RTA |
|---|---:|---:|---:|---:|
| thailand_2011 | 0 | 10.13 | 10.13 | 10.13 |
| thailand_2011 | 0.25 | 0.08 | 0.07 | 0.09 |
| thailand_2011 | 0.5 | 0.05 | 0.05 | 0.07 |
| thailand_2011 | 1 | 0.05 | 0.04 | 0.07 |
| pakistan_2022_wheat | 0 | 3.35 | 3.35 | 3.35 |
| pakistan_2022_wheat | 0.25 | 0.03 | 0.02 | 0.04 |
| pakistan_2022_wheat | 0.5 | 0.01 | 0.01 | 0.02 |
| pakistan_2022_wheat | 1 | 0.00 | 0.00 | 0.00 |
| pakistan_2022_rice | 0 | 4.67 | 4.67 | 4.67 |
| pakistan_2022_rice | 0.25 | 0.04 | 0.04 | 0.05 |
| pakistan_2022_rice | 0.5 | 0.04 | 0.04 | 0.04 |
| pakistan_2022_rice | 1 | 0.04 | 0.03 | 0.04 |
| myanmar_2008 | 0 | 4.36 | 4.36 | 4.36 |
| myanmar_2008 | 0.25 | 0.17 | 0.17 | 0.05 |
| myanmar_2008 | 0.5 | 0.10 | 0.10 | 0.04 |
| myanmar_2008 | 1 | 0.01 | 0.01 | 0.01 |
| us_midwest_2011_maize | 0 | 92.12 | 92.12 | 92.12 |
| us_midwest_2011_maize | 0.25 | 37.39 | 19.10 | 11.82 |
| us_midwest_2011_maize | 0.5 | 16.08 | 10.95 | 2.87 |
| us_midwest_2011_maize | 1 | 2.97 | 4.02 | 0.99 |
| us_midwest_2011_wheat | 0 | 27.38 | 27.38 | 27.38 |
| us_midwest_2011_wheat | 0.25 | 0.66 | 0.46 | 0.40 |
| us_midwest_2011_wheat | 0.5 | 0.29 | 0.25 | 0.26 |
| us_midwest_2011_wheat | 1 | 0.13 | 0.14 | 0.17 |

Modified PTA is the primary specification for a later flood run. At alpha 0 the published rule freezes every exporter, so other countries cannot replace a lost flow on a link that already exists. Modified PTA allows that replacement and still keeps an explicit ban closed. The world total of a pure production shock is the same under all three rules when reserves are closed, because trade cancels. What changes is who absorbs the loss.

On these placeholders, modified PTA leaves 1.8 to 9.9 percent of the consumption loss in the shocked country. Published PTA leaves about 1 percent when the shocked country is a large exporter, and much more when it is not (Pakistan wheat about 67 percent, Myanmar rice about 37 percent). RTA at alpha 0 matches published PTA. `rexp = 0.2` offers one fifth of remaining reserves on each pass, which is an offer of zero when alpha is 0.

Domestic use still absorbs 1 percent of the remaining shortage on each pass in all three settings. A production loss therefore leaves mostly through exports. That is the existing FSC step. It was not retuned.

Published PTA stays the reference for the 2020 paper. RTA stays the reserves-offer sensitivity. The export-ban rows in `allocation_comparison.csv` repeat the existing wheat and rice exporter grid (50 and 95 percent, full year and six months, four alpha values) under all three rules. `benchmark_results.csv` is unchanged and is still the published PTA grid.

The only code change inside the cascade, apart from recording the iteration count, puts trade-matrix dimnames back after `outer()` and `sweep()` drop them. The update factors are the same. Warnings that used to print a row number now print the country code.

Replace the placeholder file with observed losses and rerun `Rscript main/run_allocation_comparison.R`. The script reads that file and does not change the three allocation functions.


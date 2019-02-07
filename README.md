# Food Shocks Cascade Model
- A simple agent-based network model that computes chain-reactions due to negative production anonamlies based on dynamic food balance sheets at the country level.
- Based on original code at https://github.com/pmarchand1/cereals-network-shocks, which was described in the paper by Marchand *et al.* [Reserves and trade jointly determine exposure to food supply shocks](http://iopscience.iop.org/article/10.1088/1748-9326/11/9/095009).
- Revised version: Alison Heslin, Michael Puma (developers)

## Model Vignette
- INPUT (required): Bilateral trade of commodities at country level, production/consumption/storage for all countries
- INPUT (ancillary): Commodity list, country list, conversion factors from commodity mass to common units (e.g. kcal, protein, US dollars)
- OUTPUT: Changes in stocks and consumption at country level
- CODE: Written in R
- RUNTIME: few minutes on desktop computer
- RESOLUTION: Country level

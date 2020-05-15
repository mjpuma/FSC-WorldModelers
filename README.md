# Food Shocks Cascade Model - USA
- A simple agent-based network model that computes chain-reactions due to production anomalies based on dynamic food balance sheets at the county level in the USA.
- Adapted from the global, country-to-country version decribed in Heslin, A., M.J. Puma, P. Marchand, J.A. Carr, J. Dell'Angelo, P. D'Odorico, J.A. Gephart, M. Kummu, M. Porkka, M.C. Rulli, D. Seekell, S. Suweis, and A. Tavoni, 2020: Simulating the cascading effects of an extreme agricultural production shock: Global implications of a contemporary US Dust Bowl event. Front. Sustain. Food Syst., 20 March 2020, doi:10.3389/fsufs.2020.00026.

## Model Vignette
- INPUT (required): Bilateral trade of commodities at county level, production/consumption/storage for all counties in USA
- INPUT (ancillary): Commodity list, country list, conversion factors from commodity mass to common units (e.g. kcal, protein, US dollars)
- OUTPUT: Changes in stocks and consumption at county level
- CODE: Written in R
- RUNTIME: few minutes on desktop computer
- RESOLUTION: Country level

## Model Scripts
### main.R
This is the main script for running FSC from the command line. It wraps much of the functionality demonstrated in `Analyze.R` into a set of callable functions.

### ProcessInputs.R
Creates trade, production, and reserves matrices for use in cascade model. Requires existing files in *ancillary* and *inputs* directory: 


### FSC_component_funcs.R
Contains functions for the FSC model

### FSC_sim_funcs.R
- Example script to run the FSC model, calling functions in *FSC_component_funcs.R*

## Ancillary files
- cropcommodity_prodlist.csv = croplist for production with kcal conversions
- cropcommodity_reserveslist.csv = croplist for reserves with kcal conversions
- cropcommodity_tradelist.csv = croplist for bilateraal trade with kcal conversions
- country_list195_2012to2016.csv = country list for simulations

## Input files
- Trade data from FAOSTAT, detailed trail matrix, normalized, all data. The trade matrix is available here - http://www.fao.org/faostat/en/#data/TM - on the right side bar under "Bulk Downloads", select "All Data Normalized".   *trade_dat <- read.csv("Trade_DetailedTradeMatrix_E_All_Data_(Normalized).csv"*
- Production data from FAOSTAT, production quantity in tonnes. The crop production data are available at: http://www.fao.org/faostat/en/#data/QC. *prod_dat<-read.csv("productiondataFAOSTAT.csv")*
- Reserves data from USDA, downloadable dataset - psd grains pulses. The stocks data are available here - https://apps.fas.usda.gov/psdonline/app/index.html#/app/downloads - listed as "Grains" the file is called "psd_grains_pulses_csv.zip". *psd <- read.csv("psd_grains_pulses.csv")*


## Automatically pulling inputs
```
cd inputs
wget 'http://fenixservices.fao.org/faostat/static/bulkdownloads/Trade_DetailedTradeMatrix_E_All_Data_(Normalized).zip'
wget 'http://fenixservices.fao.org/faostat/static/bulkdownloads/Production_Crops_E_All_Data_(Normalized).zip'
wget 'https://apps.fas.usda.gov/psdonline/downloads/psd_grains_pulses_csv.zip'

unzip 'Trade_DetailedTradeMatrix_E_All_Data_(Normalized).zip'
unzip 'Production_Crops_E_All_Data_(Normalized).zip'
unzip 'psd_grains_pulses_csv.zip'
```

## Running a Simulation
To run a simulation, first clone this repository and navigate to the top-level directory with:

```
git clone git@github.com:mjpuma/FSC-WorldModelers.git
cd FSC-WorldModelers
```

Next, you need to install the required R Packages:

```
rscript main/Requirements.R
```

Next, you should prepare the input data:

```
rscript main/ProcessInputs.R
```

Now you are ready to run a simulation. To run a simulation, you should set various options in main/main.R.

# Food Shocks Cascade Model
- A simple agent-based network model that computes chain-reactions due to production anomalies based on dynamic food balance sheets at the country level.  The model is written in R.
- Latest published version of the model described in Heslin, A., M.J. Puma, P. Marchand, J.A. Carr, J. Dell'Angelo, P. D'Odorico, J.A. Gephart, M. Kummu, M. Porkka, M.C. Rulli, D. Seekell, S. Suweis, and A. Tavoni, 2020: Simulating the cascading effects of an extreme agricultural production shock: Global implications of a contemporary US Dust Bowl event. Front. Sustain. Food Syst., 20 March 2020, doi:10.3389/fsufs.2020.00026.
- Earlier model version is available at https://github.com/pmarchand1/cereals-network-shocks and described in the paper: Marchand, P., J.A. Carr, J. Dell'Angelo, M. Fader, J.A. Gephard, M. Kummu, N.R. Magliocca, M. Porkka, M.J. Puma, and Z. Ratajczak, 2016: Reserves and trade jointly determine exposure to food supply shocks. Environ. Res. Lett., 11, no. 9, 095009, doi:10.1088/1748-9326/11/9/095009.
- Resolution: Food balances/inventories computed at some speciified adminstrative level (e.g. country, province, etc) for a tme interval (e.g., one year) that is long enough for a shock to propagate through the system.
- Runtime: Preprossesing input data takes roughtly 5 minutes; cascade simulations roughtly a few minutes on desktop computer

## How to setup and run the model
### Step 1: Clone repository
To run a simulation, first clone this repository and navigate to the top-level directory with:
```
git clone git@github.com:mjpuma/FSC-WorldModelers.git
cd FSC-WorldModelers
```

### Step 2: Pull raw input files
```
cd inputs
wget 'http://fenixservices.fao.org/faostat/static/bulkdownloads/Trade_DetailedTradeMatrix_E_All_Data_(Normalized).zip'
wget 'http://fenixservices.fao.org/faostat/static/bulkdownloads/Production_Crops_E_All_Data_(Normalized).zip'
wget 'https://apps.fas.usda.gov/psdonline/downloads/psd_grains_pulses_csv.zip'

unzip 'Trade_DetailedTradeMatrix_E_All_Data_(Normalized).zip'
unzip 'Production_Crops_E_All_Data_(Normalized).zip'
unzip 'psd_grains_pulses_csv.zip'
```

### Step 3: Install the required R Packages
```
rscript main/Requirements.R
```

### Step 4: Prepare the input data
Here you need to specify the years for the baseline state of the food system (e.g., yr_range <- 2015:2017) as well as the 1)  list of countries included, 2)  set of crops commodities that will be aggregate for  bilateral trade data, 3)  set of crops commodities that will be aggregate for production data, and 4)  set of crops commodities that will be aggregate for reserves data:
  1) country_list <- read.csv("ancillary/country_list195_2012to2016.csv")
  2) commodities<-read.csv("ancillary/cropcommodity_tradelist.csv")
  3) commodities_prod<-read.csv("ancillary/cropcommodity_prodlist.csv")
  4) commodities_reserves<-read.csv("ancillary/cropcommodity_reserveslist.csv")
```
rscript main/ProcessInputs.R
```

### Step 5: Run a simulation
To run a simulation you need to specify 1) the version of the FSC, 2) the crop or set of commodity (aggregated) to simulation, and 3) the number of time steps (annual) to run.

* `FSCversion`: Specify model version to run: 0-> Run Proportional Trade Allocation (PTA) version; 
                                              1-> Run Reserves-based Trade Allocation (RTA) version
* `i_scenario`:  i_scenario -> 1 # wheat; i_scenario -> 2 # rice; i_scenario -> 3 # maize
* `num_years`: number of time steps to run consecutively (time size = 1 year) 

You can run a dynamic simulation with something like the following:
```
rscript main/main.R 0 1 5
```
In this case, we have chosen the following parameters:

* `FSCversion`: 0 -> PTA version of the FSC moodel
* `i_scenario`: 1 -> wheat commoditites
* `num_years`: 5 years

### Step 6: Output files
The model outputs are in CSV files.  There are three configurations for the output.  Below we describe the output variables and the three formats.

Type 1) Bilateral Export Matrices, where the row name is the origin country and the column is the destination. The first set of m columns is the first time step, the second set is the next timestep and so forth. Units depend on ProcessInputs.R; typically kilocalories or metric tons. 
* BilateralExportMatrix_TimeSeries.csv

Type 2) These files have four columns.  The first row is the title: iso3,	Country Name ("Country.x"),	Year,	Value. The values for each individual file are production, reserves, or food shortage (demand-supply) as indicated by the file name. These files have units consistent with the Bilateral Export Matrices.  
* Production_TimeSeries.csv
* Reserve_TimeSeries.csv
* Shortage_TimeSeries.csv

There are also two files that are unitless (i.e. norrmalized).  The first is the ratio of consumption to initial consumption and the second is the change in reserves relative to initial consumption.
* ConsumptiontoC0_TimeSeries.csv
* ReserveChangetoC0_TimeSeries.csv

Type 3: These files have network metrics computed from BilateralExportMatrix_TimeSeries.csv.  One set of files has the values from the start of the simulation and the other set has the file values.  The first row is the title.  

The set of initial value files are:
* Export_InitialTotalByCountry.csv: units consistent with export matrices.
* Import_InitialTotalByCountry.csv: units consistent with export matrices.
* NumberExportTradePartners_InitialTotalByCountry.csv: number of partners
* NumberImportTradePartners_InitialTotalByCountry.csv: number of partners

And the final value files:
* Export_FinalTotalByCountry.csv
* Import_FinalTotalByCountry.csv
* NumberExportTradePartners_FinalTotalByCountry.csv
NumberImportTradePartners_FinalTotalByCountry.csv

## Summary of Model Scripts
### main.R
This is the main script for running the *dynamic* FSC from the command line including time loop and reading of processed inputs and saving of outputs.

### main_static.R
This is the main script for running the *static* FSC from the command line.  This script simply computes changes in supply by country due to production decline anomalies (as a country list).

### ProcessInputs.R
Creates trade, production, and reserves matrices for use in the FSC model. 
Requires existing files in *ancillary* and *inputs* directory.  Processed inputs are saved and subsequently accessed by main.R or main_static.R in a separate folder called *inputs_processed*

#### Ancillary files
Commodity list, country list, conversion factors from commodity mass to common units (e.g. kcal, protein, US dollars):
- country_list195_2012to2016.csv = FAO country code, iso3 abbreviation, and full country names
- cropcommodity_tradelist.csv = Commodity list for *bilateral trade* with kcal conversions
- cropcommodity_prodlist.csv = Commodity list for *production* with kcal conversions
- cropcommodity_reserveslist.csv = Commodity list for *reserves* with kcal conversions

#### Input files
Bilateral trade of commodities at country level, production/consumption/storage for all countries, and production declines:
- Production *fractional declines* list by year by country. List depends on ancillary country list file. anomalies <- read.csv(paste0("inputs/Prod", name_crop, "_DeclineFraction_195countries.csv"))
- Trade data from FAOSTAT, detailed trail matrix, normalized, all data. The trade matrix is available here - http://www.fao.org/faostat/en/#data/TM - on the right side bar under "Bulk Downloads", select "All Data Normalized".   *trade_dat <- read.csv("Trade_DetailedTradeMatrix_E_All_Data_(Normalized).csv"*
- Production data from FAOSTAT, production quantity in tonnes. The crop production data are available at: http://www.fao.org/faostat/en/#data/QC. *prod_dat<-read.csv("productiondataFAOSTAT.csv")*
- Reserves data from USDA, downloadable dataset - psd grains pulses. The stocks data are available here - https://apps.fas.usda.gov/psdonline/app/index.html#/app/downloads - listed as "Grains" the file is called "psd_grains_pulses_csv.zip". *psd <- read.csv("psd_grains_pulses.csv")*

#### Processed input files
- Export Matrix ordered by FAOSTAT country code (increasing). *load("inputs_processed/E0.RData")*
- Production. *load("inputs_processed/P0.Rdata")* 
- Reserves. (a.k.a. Stocks) *load("inputs_processed/R0.RData")*

### FSC_sim_funcs.R
Main iteration loop to implement FSC Model 

### FSC_component_funcs.R
Functions for the FSC model
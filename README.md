# Food Shocks Cascade Model
- A simple agent-based network model that computes chain-reactions due to production anomalies based on dynamic food balance sheets at the country level.  The model is written in R.
- Latest published version of the model described in Heslin, A., M.J. Puma, P. Marchand, J.A. Carr, J. Dell'Angelo, P. D'Odorico, J.A. Gephart, M. Kummu, M. Porkka, M.C. Rulli, D. Seekell, S. Suweis, and A. Tavoni, 2020: Simulating the cascading effects of an extreme agricultural production shock: Global implications of a contemporary US Dust Bowl event. Front. Sustain. Food Syst., 20 March 2020, doi:10.3389/fsufs.2020.00026.
- Earlier model version is available at https://github.com/pmarchand1/cereals-network-shocks and described in the paper: Marchand, P., J.A. Carr, J. Dell'Angelo, M. Fader, J.A. Gephard, M. Kummu, N.R. Magliocca, M. Porkka, M.J. Puma, and Z. Ratajczak, 2016: Reserves and trade jointly determine exposure to food supply shocks. Environ. Res. Lett., 11, no. 9, 095009, doi:10.1088/1748-9326/11/9/095009.
- Resolution: Food balances/inventories computed at some specified administrative level (e.g. country, province, etc) for a time interval (e.g., one year) that is long enough for a shock to propagate through the system.
- Runtime: Preprossesing input data takes roughly 5 minutes; cascade simulations roughly a few minutes on desktop computer

## How to setup and run the model
### Step 1: Clone repository
To run a simulation, first clone this repository and navigate to the top-level directory with:
```
git clone git@github.com:mjpuma/FSC-WorldModelers.git
cd FSC-WorldModelers
```

### Step 2: Pull raw input files and place in "inputs" folder
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
As part of the FSC model, we have a Scenario Library (see below), which includes a set of scenarios that have input data already processed and ancillary files already prepared.  If you want to run one of these scenarios, you can *skip over this step*.

To customize your own simulation, you need to modify "ProcessInputs.R" and create a file with the relevant production fractional-declines list by year by country.  See the file "inputs/Scenario1_COVID_WheatDeclineFraction_1Year_195countries.csv"as an example of a 1-year run and "Scenario4_USDustBowl_WheatDeclineFraction_4Years_195countries.csv" as an example of a 4-year run. Thus, you should add a scenario to "Step 2" of main.R.

Also, you need to specify the years for the baseline state of the food system (e.g., yr_range <- 2015:2017) as well as the 1)  list of countries included, 2)  set of crops commodities that will be aggregate for  bilateral trade data, 3)  set of crops commodities that will be aggregate for production data, and 4)  set of crops commodities that will be aggregate for reserves data.  

This files are as specified below:
  1) country_list <- read.csv("ancillary/country_list195_2012to2016.csv")
  2) commodities<-read.csv("ancillary/cropcommodity_tradelist.csv")
  3) commodities_prod<-read.csv("ancillary/cropcommodity_prodlist.csv")
  4) commodities_reserves<-read.csv("ancillary/cropcommodity_reserveslist.csv")
```
rscript main/ProcessInputs.R
```

### Step 5: Run a simulation
To run a simulation you need to choose 1) the version of the FSC, 2) select a scenario from the Scenario Library, and 3) specify the fraction of "accessible" existing reserves.

The family of FSC models includes: 1) a static version without trade reallocation and 2) two dyanmic versions.

For the static version, you can run a  simulation with the following command:
```
rscript main/main_static.R
```
For the dynamic versions, you can run simulation with the following command:
```
rscript main/main.R 0 1 0.5
```
The first number (integer) following the R script name indicates the dynamic model version (`FSCversion`):
* 0 -> Run Proportional Trade Allocation (PTA) version; 
* 1 -> Run Reserves-based Trade Allocation (RTA) version

The second number (integer) indicates the simulation scenario from the Scenarios Library (see below), while the final number 
* `fractional_reserve_access`:   Parameter specifying fraction [0 to 1] of "accessible" existing reserves 


In this case, we have chosen the following parameters:

* `FSCversion`: 0 -> PTA version of the FSC model
* `i_scenario`: 1 -> A scenario for a decline in wheat production due to hypothetical COVID-19 and locust disruptions.
* `fractional_reserve_access`: 0.5 (i.e., half of each nation's reserves are accessible to buffer shock)

### Step 6: Output files
The model outputs are in CSV files.  There are three configurations for the output.  Below we describe the output variables and the three formats.

Type 1) Bilateral Export Matrices, where the row name is the origin country and the column is the destination. The first set of m columns is the first time step, the second set is the next time step and so forth. Units depend on ProcessInputs.R; typically kilocalories or metric tons. 
* BilateralExportMatrix_TimeSeries.csv

Type 2) These files have four columns.  The first row is the title: iso3,	Country Name ("Country.x"),	Year,	Value. The values for each individual file are production, reserves, or food shortage (demand-supply) as indicated by the file name. These files have units consistent with the Bilateral Export Matrices.  
* Production_TimeSeries.csv
* Reserve_TimeSeries.csv
* Shortage_TimeSeries.csv

There are also two files that are unitless (i.e. normalized).  The first is the ratio of consumption to initial consumption and the second is the change in reserves relative to initial consumption.
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


## Scenario Library
We include scenarios separately for the static FSC model (FSC-static) and the dynamic FSC model (FSC-dynamic).  Listed below are the scenarios currently included for each.


### Scenarios for the dynamic FSC model: FSC-dynamic
#### Scenario 1:  COVID-19 + locust disruption to wheat
A scenario for a hypothetical COVID-19 + locust disruption to the global wheat trade network (baseline: 2015 to 2017) for the year 2020.  This is a one-year simulation.  

The production decline fractions are: 
```
    Kenya:	0.4916
    Saudi Arabia:	0.4483
    Yemen:	0.2335
    Ukraine:	0.205
    Kazakhstan:	0.1818
    Iran:	0.1024
    Pakistan:	0.0976
    Ethiopia:	0.0874
    Russia:	0.0768
```

#### Scenario 2:  COVID-19 + locust disruption to maize
A scenario for a hypothetical COVID-19 + locust disruption to the global maize trade network (baseline: 2015 to 2017) for the year 2020.  This is a one-year simulation.  

The production decline fractions are:
```
    Saudi Arabia	0.5281
    Somalia	 	0.4844
    Iran	0.2975
    Yemen	0.2518
    Kenya	0.1521
    Brazil	0.1328
    Ethiopia	0.1301
    Argentina	0.119
    Ukraine	0.0878
    Pakistan	0.0717
```
  
#### Scenario 3:  COVID-19 + locust disruption to rice
A scenario for a hypothetical COVID-19 + locust disruption to the global rice trade network (baseline: 2015 to 2017) for the year 2020.  This is a one-year simulation.  

The production decline fractions are:
```
    Kenya	0.3425
    Pakistan	0.197
    Iran	0.1883
    Thailand	0.021
    India	0.0192
    Viet Nam	0.0012
```
  
#### Scenario 4:  US Dust Bowl disruption to wheat
A scenario for a hypothetical US Dust Bowl analogue event to the global wheat trade network (baseline: 2015 to 2017) for the year 2020.  This is a four-year simulation.  The production decline fractions are: 

    USA: 0.33	0.36	0.24	0.24 (for years 1 to 4, respectively)
    
 
### Scenarios for the static FSC model: FSC-static
#### Scenario 1: 
  name_crop <-c('Wheat')
  runname <- c('Wheat_Avg20152017')
  nameinput <- c('Wheat_Avg20152017')
  shock_scenario <- read.csv(paste0("inputs/Scenario1_COVID_WheatDeclineFraction_1Year_195countries.csv"))
  
#### Scenario 2: 
  name_crop <-c('Maize')
  runname <- c('Maize_Avg20152017')
  nameinput <- c('Maize_Avg20152017')
  shock_scenario <- read.csv(paste0("inputs/Scenario2_COVID_MaizeDeclineFraction_1Year_195countries.csv"))
  
#### Scenario 3: 
  name_crop <-c('Rice')
  runname <- c('Rice_Avg20152017')
  nameinput <- c('Rice_Avg20152017')
  shock_scenario <- read.csv(paste0("inputs/Scenario3_COVID_RiceDeclineFraction_1Year_195countries.csv"))

#### Scenario 11: 
  name_crop <-c('Wheat')
  runname <- c('Wheat_breadbasket_Year2008')
  nameinput <- c('Wheat_Avg20152017')
  shock_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Wheat_DeclineFraction_breadbasket_Year2008_195countries.csv"))
  #export_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))

#### Scenario 12: 
  name_crop <-c('Wheat')
  runname <- c('Wheat_locust_Year2020')
  nameinput <- c('Wheat_Avg20152017')
  shock_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Wheat_DeclineFraction_locust_Year2020_195countries.csv"))
  #export_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))
  
#### Scenario 13: 
  name_crop <-c('Wheat')
  runname <- c('Wheat_locust_Year2020_breadbasket_Year2008')
  nameinput <- c('Wheat_Avg20152017')
  shock_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Wheat_DeclineFraction_locust_Year2020_breadbasket_year2008_195countries.csv"))
  #export_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))

#### Scenario 14: 
  name_crop <-c('Wheat')
  runname <- c('Wheat_ExportRestrictionFraction_Year2008')
  nameinput <- c('Wheat_Avg20152017')
  shock_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))
  #export_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Wheat_ExportRestrictionFraction_Year2008_195countries.csv"))

#### Scenario 15: 
  name_crop <-c('Wheat')
  runname <- c('Wheat_ExportRestrictionFraction_Year2008with_RUS_25')
  nameinput <- c('Wheat_Avg20152017')
  shock_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))
  #export_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Wheat_ExportRestrictionFraction_Year2008_with_RUS_25%_195countries.csv"))

#### Scenario 16: 
  name_crop <-c('Wheat')
  runname <- c('Wheat_ExportRestrictionFraction_Year2008with_RUS_50')
  nameinput <- c('Wheat_Avg20152017')
  shock_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))
  #export_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Wheat_ExportRestrictionFraction_Year2008_with_RUS_50%_195countries.csv"))

#### Scenario 21:
  name_crop <-c('Maize')
  runname <- c('Maize_breadbasket_Year2008')
  nameinput <- c('Maize_Avg20152017')
  shock_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Maize_DeclineFraction_breadbasket_Year2008_195countries.csv"))
  #export_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))

#### Scenario 22:
  name_crop <-c('Maize')
  runname <- c('Maize_locust_Year2020')
  nameinput <- c('Maize_Avg20152017')
  shock_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Maize_DeclineFraction_locust_Year2020_195countries.csv"))
  #export_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))
  
#### Scenario 23:
  name_crop <-c('Maize')
  runname <- c('Maize_locust_Year2020_breadbasket_Year2008')
  nameinput <- c('Maize_Avg20152017')
  shock_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Maize_DeclineFraction_locust_Year2020_breadbasket_year2008_195countries.csv"))
  #export_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))

#### Scenario 24:
  name_crop <-c('Maize')
  runname <- c('Maize_ExportRestrictionFraction_Year2008')
  nameinput <- c('Maize_Avg20152017')
  shock_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))
  #export_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Maize_ExportRestrictionFraction_Year2008_195countries.csv"))

#### Scenario 25:
  name_crop <-c('Maize')
  runname <- c('Maize_ExportRestrictionFraction_Year2008with_ARG_25')
  nameinput <- c('Maize_Avg20152017')
  shock_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))
  #export_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Maize_ExportRestrictionFraction_Year2008_with_ARG_25%_195countries.csv"))

#### Scenario 26:
  name_crop <-c('Maize')
  runname <- c('Maize_ExportRestrictionFraction_Year2008with_ARG_50')
  nameinput <- c('Maize_Avg20152017')
  shock_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))
  #export_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Maize_ExportRestrictionFraction_Year2008_with_ARG_50%_195countries.csv"))

#### Scenario 31:
  name_crop <-c('Rice')
  runname <- c('Rice_breadbasket_Year2008')
  nameinput <- c('Rice_Avg20152017')
  shock_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Rice_DeclineFraction_breadbasket_Year2008_195countries.csv"))
  #export_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))

#### Scenario 32:
  name_crop <-c('Rice')
  runname <- c('Rice_locust_Year2020')
  nameinput <- c('Rice_Avg20152017')
  shock_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Rice_DeclineFraction_locust_Year2020_195countries.csv"))
  #export_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))
  
#### Scenario 33:
  name_crop <-c('Rice')
  runname <- c('Rice_locust_Year2020_breadbasket_Year2008')
  nameinput <- c('Rice_Avg20152017')
  shock_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Rice_DeclineFraction_locust_Year2020_breadbasket_year2008_195countries.csv"))
  #export_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))

#### Scenario 34:
  name_crop <-c('Rice')
  runname <- c('Rice_ExportRestrictionFraction_Year2008')
  nameinput <- c('Rice_Avg20152017')
  shock_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))
  #export_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Rice_ExportRestrictionFraction_Year2008_195countries.csv"))

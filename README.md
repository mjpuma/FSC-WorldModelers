# Food Shocks Cascade Model
- A simple agent-based network model that computes chain-reactions due to production anomalies based on dynamic food balance sheets at the country level.
- Development version of the model introduced and described in Heslin, A., M.J. Puma, P. Marchand, J.A. Carr, J. Dell'Angelo, P. D'Odorico, J.A. Gephart, M. Kummu, M. Porkka, M.C. Rulli, D. Seekell, S. Suweis, and A. Tavoni, 2020: Simulating the cascading effects of an extreme agricultural production shock: Global implications of a contemporary US Dust Bowl event. Front. Sustain. Food Syst., 20 March 2020, doi:10.3389/fsufs.2020.00026.
- Earlier model version is available at https://github.com/pmarchand1/cereals-network-shocks and described in the paper: Marchand, P., J.A. Carr, J. Dell'Angelo, M. Fader, J.A. Gephard, M. Kummu, N.R. Magliocca, M. Porkka, M.J. Puma, and Z. Ratajczak, 2016: Reserves and trade jointly determine exposure to food supply shocks. Environ. Res. Lett., 11, no. 9, 095009, doi:10.1088/1748-9326/11/9/095009.

## Model Vignette
- INPUT (required): Bilateral trade of commodities at country level, production/consumption/storage for all countries
- INPUT (ancillary): Commodity list, country list, conversion factors from commodity mass to common units (e.g. kcal, protein, US dollars)
- OUTPUT: Changes in stocks and consumption at country level
- CODE: Written in R
- RUNTIME: few minutes on desktop computer
- RESOLUTION: Country level

## Model Scripts
### main.R
This is the main script for running the *dynamic* FSC from the command line including time loop and reading of processed inputs and saving of outputs.

### main_static.R
This is the main script for running the *static* FSC from the command line.  This script simply computes changes in country supplies due to production decline anomalies (as a country list).

### ProcessInputs.R
Creates trade, production, and reserves matrices for use in the FSC model. 
Requires existing files in *ancillary* and *inputs* directory: 
#### Ancillary files
- country_list195_2012to2016.csv = FAO country code, iso3 abbreviation, and full country names
- cropcommodity_tradelist.csv = Commodity list for *bilateral trade* with kcal conversions
- cropcommodity_prodlist.csv = Commodity list for *production* with kcal conversions
- cropcommodity_reserveslist.csv = Commodity list for *reserves* with kcal conversions

#### Input files
- Production *fractional declines* list by year by country. List depends on ancillary country list file. anomalies <- read.csv(paste0("inputs/Prod", name_crop, "_DeclineFraction_195countries.csv"))
- Trade data from FAOSTAT, detailed trail matrix, normalized, all data. The trade matrix is available here - http://www.fao.org/faostat/en/#data/TM - on the right side bar under "Bulk Downloads", select "All Data Normalized".   *trade_dat <- read.csv("Trade_DetailedTradeMatrix_E_All_Data_(Normalized).csv"*
- Production data from FAOSTAT, production quantity in tonnes. The crop production data are available at: http://www.fao.org/faostat/en/#data/QC. *prod_dat<-read.csv("productiondataFAOSTAT.csv")*
- Reserves data from USDA, downloadable dataset - psd grains pulses. The stocks data are available here - https://apps.fas.usda.gov/psdonline/app/index.html#/app/downloads - listed as "Grains" the file is called "psd_grains_pulses_csv.zip". *psd <- read.csv("psd_grains_pulses.csv")*

### FSC_sim_funcs.R
Main iteration loop to implement FSC Model 

### FSC_component_funcs.R
Functions for the FSC model


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

## Running a dynamic simulation
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

Now you are ready to run a simulation. To run a simulation you should choose the following:

* `FSCversion`: Specify model version to run: 0-> Run Proportional Trade Allocation (PTA) version; 
                                              1-> Run Reserves-based Trade Allocation (RTA) version
* `i_scenario`:  i_scenario -> 0  # normal (default wheat) FSC run without exogenous restriction
                 i_scenario -> 1 # wheat; i_scenario -> 2 # rice; i_scenario -> 3 # maize

Now, you can run a dynamic simulation with something like the following:

```
rscript main/main.R 0 0
```
In this case, we have chosen the following parameters:

* `FSCversion`: 0
* `i_scenario`: 0


## Running with Docker
First, you must build the Docker image. To do this, navigate to the top-level of this repository and then run:

```
docker build -t fsc/latest .
```

Here, we have tagged the container as `fsc/latest`. Once the container is built we can run it by mounting an `outputs` directory from our host to the container and then passing the appropriate arguments in the correct order:

```
docker run -v $PWD/outputs:/outputs fsc/latest 0 0

```

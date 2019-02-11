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

## Model Scripts
### MatrixCreation.R
Creates trade, production, and reserves matrices for use in cascade model. Requires existing files in *ancillary* and *inputs* directory: 

### CascadeFunction.R
Contains functions for the FSC model

### Analyze.R
- Example script to run the FSC model, calling functions in *CascadeFunction.R* for the case where production 1) in a single country is negatively affected and 2) in multiple countries are negatively affected.

## Ancillary files
- crop_list.csv = croplist with kcal conversions
- country_list.csv = FAO country code and true/false for country pop over 500k 
- ciso3.txt = country codes

## Input files
- Production data from FAOSTAT, production quantity in tonnes (prod_dat<-read.csv("productiondataFAOSTAT.csv"))
- Trade data from FAOSTAT, detailed trail matrix, normalized, all data (trade_dat <- read.csv("Trade_DetailedTradeMatrix_E_All_Data_(Normalized).csv")
- Reserves data from USDA, downloadable dataset - psd grains pulses (psd <- read.csv("psd_grains_pulses.csv"))

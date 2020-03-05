## Process *structured* input datasets ##
## Creates trade, production, and reserves matrices for use in cascade model ##

## Requires existing files in working directory: 
## Commodity List (with kcal conversions), 
## Production data from FAOSTAT, production quantity in tonnes 
## Trade data from FAOSTAT, detailed trail matrix, normalized, all data 
## Reserves data from USDA, downloadable dataset - psd grains pulses 

library(tidyr)
library(dplyr)
library(reshape2)
library(igraph)
library(stringr)
#library(countrycode)

# set year range for production, trade, and reserves data
yr_range <- 2012:2016

topdir <- getwd()

# Load ancillary data
# 1)  Commodity list 
commodities<-read.csv("ancillary/cropcommodity_list.csv")
# 2) Load country list
country_list <- read.csv("ancillary/country_list195.csv")


###########################################################################
### PART 1: Trade (Detailed trade matrix pre-downloaded trade from FAOSTAT)
## download detailed trade matrix from FAO 
###########################################################################

trade_dat <- read.csv("inputs/Trade_DetailedTradeMatrix_E_All_Data_(Normalized).csv",
                      stringsAsFactors = FALSE)

trade_dat <- select(trade_dat, reporter = Reporter.Country.Code, 
                    ReporterName = Reporter.Countries,
                    partner = Partner.Country.Code, 
                    PartnerName = Partner.Countries, 
                    cropid = Item.Code, 
                    element = Element,
                    year = Year, 
                    value = Value) %>%
  filter(year %in% yr_range)

#  Filter for wheat and wheat products 
# trade_dat<-trade_dat[(trade_dat$cropid==15 | 
#                         trade_dat$cropid==16 |
#                         trade_dat$cropid==18 |
#                         trade_dat$cropid==19|
#                         trade_dat$cropid==20 |
#                         trade_dat$cropid==21|
#                         trade_dat$cropid==22),]

## Crop list adjustments
# # Put item 30 (rice total - milled eq.) under 31 (rice milled)
# trade_dat$cropid[trade_dat$cropid == 30] <- 31

## Country list adjustments
# 1) China note: combine Hong Kong, Macau, mainland China and Taiwan?
# Put parts of China under same ID
# china_ids <- c(41, 96, 128, 214)
# trade_dat$reporter[trade_dat$reporter %in% china_ids] <- 351
# trade_dat$partner[trade_dat$partner %in% china_ids] <- 351
# rm(china_ids)
# 2) Sudan note: before 2012, Sudan code is 206; in 2012, it split  to Sudan (276) and South Sudan (277)

## Keep countries in country_list and remove trade between a country and itself
#trade_dat <- filter(trade_dat, reporter %in% country_list$FAOST_CODE,
#                    partner %in% country_list$FAOST_CODE) %>%
#    filter(reporter != partner)

# Remove trade between a country and itself
trade_dat <- filter(trade_dat, reporter != partner)

# Extract exports
exp_dat <- filter(trade_dat, element == "Export Quantity") %>% select(-element)

# Convert to kcal and aggregate across crops
exp_agg <- inner_join(exp_dat, commodities) %>%
  mutate(value_kcal = value * as.numeric(kcal.ton)) %>%
  group_by(reporter, partner, year) %>%
  summarise(ekcal = sum(value_kcal, na.rm = TRUE))

# # Create country list from trade data
# exp_dat$reporter[!(exp_dat$reporter %in% exp_dat$partner)] #check all countries from matrix are captured in "partner"
# exp_dat$PartnerName[exp_dat$partner==351]<-"China"
# countrylist<-data.frame(CountryCode=unique(exp_dat$partner), CountryName=unique(exp_dat$PartnerName))
# countrylist$CountryName<-as.character(countrylist$CountryName)
# countrylist$CountryName[countrylist$CountryCode==107]<-"Cote d'Ivoire"
# countrylist<-countrylist[order(countrylist$CountryName),]

# Convert export dataframe to a country x country x year matrix
exp_mat <- acast(exp_agg, reporter ~ partner ~ year)
exp_mat <- exp_mat[match(country_list$FAOST_CODE, dimnames(exp_mat)[[1]]), , ]
exp_mat <- exp_mat[, match(country_list$FAOST_CODE, dimnames(exp_mat)[[2]]), ]
exp_mat[is.na(exp_mat)] <- 0
dimnames(exp_mat) <- list(country_list$FAOST_CODE, country_list$FAOST_CODE, yr_range)
E0 <- apply(exp_mat[, , 1:5], 1:2, mean) ###Remove before commit (assumes fixed length 1:5)

# # Remove countries with no trade data
# no_dat <- which(rowSums(E0) == 0 & colSums(E0) == 0)
# E0 <- E0[-no_dat, -no_dat]
# Countries<-data_frame(FAO=dimnames(E0)[[1]])
# Countries<-merge(Countries, countrylist, by.x='FAO',by.y='CountryCode')
# Countries$iso3c<-countrycode(Countries$FAO, "fao", "iso3c")
# write.csv(Countries, "TradeCountries.csv", row.names = FALSE)
# Countries<-Countries[order(Countries$CountryName),]
# dimnames(E0) <- list(Countries$iso3c, Countries$iso3c)
# E0<-E0[!is.na(rownames(E0)),]
# E0<-E0[,!is.na(colnames(E0))]

save(E0, file="inputs_processed/E0.RData")

##to export matrices with country names
# Tkbyc<-as.data.frame(E0, row.names = dimnames(E0)[[1]], col.names = dimnames(E0)[[2]] )
Tkbyc<-as.data.frame(E0, row.names = country_list$Country, col.names = dimnames(E0)[[2]] )
write.csv(Tkbyc, "inputs_processed/E0.csv")

###########################################################################
## PART 2 : Get production data from FAOSTAT
## QC: production-crops domain, 5510: production in tonnes
###########################################################################
setwd("~/GitHub_mjpuma/FSC-WorldModelers/")            ###Remove before commit
topdir <- getwd()                                      ###Remove before commit

# Download production data from FAOSTAT, import
prod_dat<-read.csv("inputs/Production_Crops_E_All_Data_(Normalized).csv")

# Rename to match functions code
prod_dat$FAO<-prod_dat$Area.Code
prod_dat$itemCode<-prod_dat$Item.Code
prod_dat$FAOST_CODE<-prod_dat$Area.Code
prod_dat$name<-prod_dat$Item

# Keep only relevant columns
prod_dat <- prod_dat[, c("FAOST_CODE", "Year", "Value", "itemCode", "name")]

#prod_dat<-prod_dat %>%
#  filter(   Item.Code==15)

# Keep only relevant years
prod_dat<-prod_dat  %>%
  filter(Year %in% yr_range)

# Add kcal conversion factor,
# then sum calories by country and year across different cereals
prod_agg <- inner_join(prod_dat, commodities, by = c("itemCode" = "cropid")) %>%
  mutate(value_kcal = Value * as.numeric(kcal.ton)) %>%
  group_by(FAOST_CODE, Year) %>%
  summarise(pkcal = sum(value_kcal, na.rm = TRUE))

# Convert to country x year matrix
prod_mat <- spread(prod_agg, key = Year, value = pkcal, fill = 0)
prod_mat <- prod_mat[match(country_list$FAOST_CODE, prod_mat$FAOST_CODE), ] #expand to include all countries
prod_mat <- as.matrix(prod_mat[, -1]) #remove country code column
prod_mat[is.na(prod_mat)] <- 0 #replace NAs with 0

P0 <- rowMeans(prod_mat[, 1:5])
#P0<-P0[!is.na(names(P0))]
save(P0, E0, file="inputs_processed/P0E0.RData")
save(P0, file= "inputs_processed/P0.Rdata")

colnames(prod_mat) <- NULL
Pkbyc <- prod_mat
##to export matrix with country names
Pkbyc<-data_frame(country_list$Country, P0=P0)
write.csv(Pkbyc,"inputs_processed/Production.csv", row.names=FALSE)

# Clear unneeded files
rm(prod_dat, prod_agg)

###########################################################################
##Part 3: Stocks of cereals in countries x years matrix 
### USDA Foreign Agricultural Service 
### Production, Supply, and Distribution (PSD) data
### download USDA-PSD data pulses grains
###########################################################################
setwd("~/GitHub_mjpuma/FSC-WorldModelers/")            ###Remove before commit
topdir <- getwd()                                      ###Remove before commit

## (1) Load data and get reserves in kcal
psd <- read.csv("inputs/psd_grains_pulses.csv")

# Extract year-end stocks for specified year range (units: 1000 metric tons)
psd_yrsubset <- select(psd, Country = Country_Name, Year = Market_Year,
                  Product = Commodity_Description,
                  Variable = Attribute_Description, Value) %>%
  filter(Year %in% yr_range) %>%
  spread(key = Variable, value = Value) %>%
  select(Country, Year, Product, R = `Ending Stocks`)

# Read table of commodities and kcal/tonnes conversion factors 
commodities_usda_names <- commodities

# Adjust names to match PSD database (the second name is the one used in PSD)
# The commodities included for 'Ending Stocks' are: Barley, Corn, Millet, Mixed Grain,
# Oats, "Rice, Milled", Rye, Sorghum, Wheat
subs_list <- c("Rice Milled" = "Rice, Milled", 
               "Maize" = "Corn", 
               "Mixed grain" = "Mixed Grain")
commodities_usda_names$cropname <- str_replace_all(commodities_usda_names$cropname, subs_list)

# Calculate Reserves in kcal, reshape to Country x Year
Rkbyc <- inner_join(psd_yrsubset, commodities_usda_names, by = c("Product" = "cropname")) %>%
    select(Country, Year, Product, R, convf = kcal.ton) %>%
    mutate(R = R * 1000 * as.numeric(convf)) %>%
    group_by(Country, Year) %>%
    summarise(R = sum(R, na.rm = TRUE)) %>%
    spread(key = Year, value = R, fill = 0)

## (2) Various fixes to ensure countries match between FAOSTAT and PSD data
# Fix non-matching country names
countries_to_match <- unique(Rkbyc$Country[!(Rkbyc$Country %in% country_list$Country)])
countries_to_match
country_repl <- c("Bolivia" = "Bolivia (Plurinational State of)", 
                  "Brunei" =  "Brunei Darussalam", 
                  "Burkina" =  "Burkina Faso",
                  "Burma" =   "Myanmar", 
                  "Former Czechoslovakia" = "Czechoslovakia", 
                  "Former Yugoslavia" = "Yugoslav SFR", 
                  "Gambia, The" = "Gambia",
                  "Iran" = "Iran (Islamic Republic of)", 
                  "Korea, North" = "Democratic People's Republic of Korea", 
                  "Korea, South" =  "Republic of Korea", 
                  "Laos" = "Lao People's Democratic Republic",
                  "Macedonia" = "The former Yugoslav Republic of Macedonia",
                  "Moldova" = "Republic of Moldova", 
                  "Russia" =  "Russian Federation", 
                  "Syria" = "Syrian Arab Republic",
                  "Tanzania" = "United Republic of Tanzania", 
                  "Union of Soviet Socialist Repu" = "USSR", 
                  "United States" = "United States of America", 
                  "Venezuela" =  "Venezuela (Bolivarian Republic of)", 
                  "Vietnam" = "Viet Nam")
Rkbyc$Country <- str_replace_all(Rkbyc$Country, country_repl)
Rkbyc$Country <- str_replace(Rkbyc$Country, fixed("Congo (Brazzaville)"), "Congo") %>%
  str_replace(fixed("Congo (Kinshasa)"), "Democratic Republic of the Congo") %>%
  str_replace(fixed("Yemen (Aden)"), "Yemen Dem") %>%
  str_replace(fixed("Yemen (Sanaa)"), "Yemen Ar Rp")

cnames <- Rkbyc$Country
Rkbyc <- as.matrix(Rkbyc[, -1,])
rownames(Rkbyc) <- cnames

## Country  list adjustments
# Sudan note: before 2012, Sudan code is 206; in 2012, it split 
# to Sudan (276) and South Sudan (277)

# China note: Add Taiwan and HK to China, South Sudan to Sudan, and two Yemens
#Rkbyc["China", ] <- Rkbyc["China", ] + Rkbyc["Hong Kong", ] + Rkbyc["Taiwan", ]
#Rkbyc["Sudan", ] <- Rkbyc["Sudan", ] + Rkbyc["South Sudan", ] # Sudna only up to 2011
#Rkbyc["Yemen", ] <- Rkbyc["Yemen", ] + Rkbyc["Yemen Ar Rp", ] + Rkbyc["Yemen Dem", ]
#Rkbyc <- Rkbyc[!(rownames(Rkbyc) %in% c("Hong Kong", "Taiwan", "South Sudan", 
#                                        "Yemen Ar Rp", "Yemen Dem")), ]

## Apportion European Union reserves proportionally to production
eu_list <- c("Austria", 
             "Belgium", 
             "Denmark", 
             "Finland", 
             "France", 
             "Germany",
             "Greece", 
             "Ireland", 
             "Italy", 
             "Luxembourg", 
             "Netherlands",
             "Portugal", 
             "Spain", 
             "Sweden", 
             "United Kingdom",
             "Bulgaria", 
             "Croatia", 
             "Cyprus", 
             "Czech Republic",
             "Estonia", 
             "Hungary", 
             "Latvia", 
             "Lithuania", 
             "Poland",
             "Romania", 
             "Slovakia", 
             "Slovenia")
eu_yrs <- which(as.numeric(colnames(Rkbyc)) >= 1998)

# Label rows in production matrix from from country_list
Pkbyc_v2<-prod_mat
rownames(Pkbyc_v2) <- country_list$Country
prop_eu <- scale(Pkbyc_v2[eu_list, eu_yrs], center = FALSE,
                 scale = colSums(Pkbyc_v2[eu_list, eu_yrs]))

# Add individual EU countries to matrix
Rkbyc <- rbind(Rkbyc, matrix(0, nrow = length(eu_list), ncol = ncol(Rkbyc), 
                             dimnames = list(eu_list)))
Rkbyc[eu_list, eu_yrs] <- sweep(prop_eu, 2, Rkbyc["European Union", eu_yrs], "*") 
Rkbyc <- Rkbyc[-which(rownames(Rkbyc) %in% c("European Union")), ] #delete EU row

## (4) Create new matrix with all countries in Pkbyc (full country list),
# fill reserves by matching names, leave the rest at zero
Rkbyc_all<-Rkbyc
Rkbyc_all <- Rkbyc_all[match(country_list$Country, dimnames(Rkbyc_all)[[1]]),]
Rkbyc_all[is.na(Rkbyc_all)] <- 0

rownames(Rkbyc_all)<-country_list$Country
dimnames(Rkbyc_all)<-NULL
saveRDS(Rkbyc_all, file = "inputs_processed/cereals_stocks.RData")

# Take average years and save as R0
dimnames(Rkbyc_all) <- list(country_list$Country, yr_range)
R0 <- rowMeans(Rkbyc_all[, eu_yrs])
R0<-R0[!is.na(names(R0))]
save(R0, file = "inputs_processed/R0.RData")

Rkbyc<-data_frame(iso3=names(R0), R0=R0)
write.csv(Rkbyc,"inputs_processed/Reserves.csv", row.names=FALSE)

### clear environment
rm(list=ls()) 
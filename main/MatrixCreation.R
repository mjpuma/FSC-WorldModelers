                 ## Matrix creation ##
## Creates trade, production, and reserves matrices for use in cascade model ##


## Requires existing files in working directory: 
## croplist (with kcal conversions), 
## countrylist (with FAO country code, and true/false for country pop over 500k), 
## Production data from FAOSTAT, production quantity in tonnes 
## Trade data from FAOSTAT, detailed trail matrix, normalized, all data 
## Reserves data from USDA, downloadable dataset - psd grains pulses 


#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("reshape2")
#install.packages("igraph")
#install.packages("stringr")

library(tidyr)
library(dplyr)
library(reshape2)
library(igraph)
library(stringr)


setwd("~/Dropbox/MURI/NuclearWinter/FSC-Model/")
#setwd("/Users/puma/Dropbox/PumaModels/FSC-Model/")

#load crop list, restrict to cereal crops (group_id=1)

crop_list <- read.delim("ancillary/crop_list.tsv", header = TRUE, as.is = TRUE) %>%
    filter(Groupnum == 1) # restrict to Cereals, group==1
prod_crops <- filter(crop_list, ProductionL == 1, cropname != "Bulgur") %>%
    select(cropid, cropname, kcal.ton) #restrict to 'primary' crops, ProductionL==1
trade_crops <- filter(crop_list, TradeL == 1) %>%
    select(cropid, kcal.ton)
country_list <- read.csv("ancillary/country_list.csv")
yr_range <- 2000:2010
ciso <- read.table("ancillary/ciso3.txt", stringsAsFactors = FALSE)


###########################################################################
## PART 1 : Get production data from FAOSTAT
## QC: production-crops domain, 5510: production in tonnes
###########################################################################

# download production data from FAOSTAT, import  
prod_dat<-read.csv("inputs/productiondataFAOSTAT.csv")

# rename to match functions code, keep only relevant columns
prod_dat$itemCode<-prod_dat$Item.Code
prod_dat$FAOST_CODE<-prod_dat$Area.Code
prod_dat$name<-prod_dat$Item
prod_dat <- prod_dat[, c("FAOST_CODE", "Year", "Value", "itemCode", "name")]
prod_dat<-prod_dat  %>%
    filter(Year %in% yr_range)

# Add kcal conversion factor, 
# then sum calories by country and year across different cereals
prod_agg <- inner_join(prod_dat, prod_crops, by = c("itemCode" = "cropid")) %>%
    mutate(value_kcal = Value * as.numeric(kcal.ton)) %>%
    group_by(FAOST_CODE, Year) %>%
    summarise(pkcal = sum(value_kcal, na.rm = TRUE))

# Convert to country x year matrix
prod_mat <- spread(prod_agg, key = Year, value = pkcal, fill = 0)
prod_mat <- prod_mat[match(country_list$FAOST_CODE, prod_mat$FAOST_CODE), ] #expand to include all countries 
prod_mat <- as.matrix(prod_mat[, -1]) #remove country code column
prod_mat[is.na(prod_mat)] <- 0 #replace NAs with 0
colnames(prod_mat) <- NULL

# Only keep data for country with pop > 500k
prod_mat[!country_list$gt.500k, ] <- 0

# Clear unneeded files
rm(prod_dat, prod_agg)

Pkbyc <- prod_mat
###########################################################################
### PART 2: Trade (Detailed trade matrix pre-downloaded trade from FAOSTAT)
## download detailed trade matrix from FAO 
###########################################################################

trade_dat <- read.csv("inputs/Trade_DetailedTradeMatrix_E_All_Data_(Normalized).csv",
                      stringsAsFactors = FALSE)

trade_dat <- select(trade_dat, reporter = Reporter.Country.Code, 
                    partner = Partner.Country.Code, cropid = Item.Code, 
                    element = Element, year = Year, value = Value) %>%
    filter(year %in% yr_range)

# Put parts of China under same ID
china_ids <- c(41, 96, 128, 214)
trade_dat$reporter[trade_dat$reporter %in% china_ids] <- 351
trade_dat$partner[trade_dat$partner %in% china_ids] <- 351
rm(china_ids)

# Put item 30 (rice total - milled eq.) under 31 (rice milled)
trade_dat$cropid[trade_dat$cropid == 30] <- 31

# Keep countries in country_list and remove trade between a country and itself
trade_dat <- filter(trade_dat, reporter %in% country_list$FAOST_CODE,
                    partner %in% country_list$FAOST_CODE) %>%
    filter(reporter != partner)

# Extract exports
exp_dat <- filter(trade_dat, element == "Export Quantity") %>% select(-element)

# Convert to kcal and aggregate across crops
exp_agg <- inner_join(exp_dat, trade_crops) %>%
    mutate(value_kcal = value * as.numeric(kcal.ton)) %>%
    group_by(reporter, partner, year) %>%
    summarise(ekcal = sum(value_kcal, na.rm = TRUE))


# Convert export datafarme to a country x country x year matrix
exp_mat <- acast(exp_agg, reporter ~ partner ~ year)
exp_mat <- exp_mat[match(ciso$FAO, dimnames(exp_mat)[[1]]), , ]
exp_mat <- exp_mat[, match(ciso$FAO, dimnames(exp_mat)[[2]]), ]
exp_mat[is.na(exp_mat)] <- 0

# Remove countries where max population < 500k over time period
exp_mat[!country_list$gt.500k, , ] <- 0
exp_mat[, !country_list$gt.500k, ] <- 0

dimnames(exp_mat) <- NULL

Tkbyc <- exp_mat
save(Pkbyc, Tkbyc, file="inputs/cereals_prod_trade.RData")

###########################################################################
##Part 3: Stocks of cereals in countries x years matrix 
### download USDA-PSD data pulses grains
###########################################################################

## (1) Load data and get reserves in kcal
psd <- read.csv("inputs/psd_grains_pulses.csv")

# Extract year-end stocks 2000-2010 (units: 1000 metric tons)
psd0010 <- select(psd, Country = Country_Name, Year = Market_Year,
                  Product = Commodity_Description,
                  Variable = Attribute_Description, Value) %>%
    filter(Year %in% 2000:2010) %>%
    spread(key = Variable, value = Value) %>%
    select(Country, Year, Product, R = `Ending Stocks`)

# Read table of kcal/tonnes conversion factors and match commodity names
conv_kcal <- crop_list
subs_list <- c("Rice Milled" = "Rice, Milled", "Maize" = "Corn", 
               "Mixed grain" = "Mixed Grain")
conv_kcal$cropname <- str_replace_all(conv_kcal$cropname, subs_list)

# Calculate R in kcal, reshape to Country x Year
Rkbyc <- inner_join(psd0010, conv_kcal, by = c("Product" = "cropname")) %>%
    select(Country, Year, Product, R, convf = kcal.ton) %>%
    mutate(R = R * 1000 * as.numeric(convf)) %>%
    group_by(Country, Year) %>%
    summarise(R = sum(R, na.rm = TRUE)) %>%
    spread(key = Year, value = R, fill = 0)

## (2) Various fixes to ensure countries match between FAOSTAT and PSD data

# Fix non-matching country names
countries_to_match <- unique(Rkbyc$Country[!(Rkbyc$Country %in% ciso$name)])
country_repl <- c("Bolivia" = "Bolivia (Plurinational State of)", "Brunei" = 
                      "Brunei Darussalam", "Burkina" =  "Burkina Faso", "Burma" = 
                      "Myanmar", "Former Czechoslovakia" = "Czechoslovakia", 
                  "Former Yugoslavia" = "Yugoslav SFR", "Gambia, The" = "Gambia",
                  "Iran" = "Iran (Islamic Republic of)", "Korea, North" = 
                      "Democratic People's Republic of Korea", "Korea, South" = 
                      "Republic of Korea", "Laos" = "Lao People's Democratic Republic",
                  "Macedonia" = "The former Yugoslav Republic of Macedonia",
                  "Moldova" = "Republic of Moldova", "Russia" = 
                      "Russian Federation", "Syria" = "Syrian Arab Republic",
                  "Tanzania" = "United Republic of Tanzania", 
                  "Union of Soviet Socialist Repu" = "USSR", "United States" =
                      "United States of America", "Venezuela" = 
                      "Venezuela (Bolivarian Republic of)", "Vietnam" = "Viet Nam")
Rkbyc$Country <- str_replace_all(Rkbyc$Country, country_repl)
Rkbyc$Country <- str_replace(Rkbyc$Country, fixed("Congo (Brazzaville)"), "Congo") %>%
    str_replace(fixed("Congo (Kinshasa)"), "Democratic Republic of the Congo") %>%
    str_replace(fixed("Yemen (Aden)"), "Yemen Dem") %>%
    str_replace(fixed("Yemen (Sanaa)"), "Yemen Ar Rp")
cnames <- Rkbyc$Country
Rkbyc <- as.matrix(Rkbyc[, -1])
rownames(Rkbyc) <- cnames

# Add Taiwan and HK to China, South Sudan to Sudan, and two Yemens
Rkbyc["China", ] <- Rkbyc["China", ] + Rkbyc["Hong Kong", ] + Rkbyc["Taiwan", ]
#Rkbyc["Sudan", ] <- Rkbyc["Sudan", ] + Rkbyc["South Sudan", ] #data end in 2010
#Rkbyc["Yemen", ] <- Rkbyc["Yemen", ] + Rkbyc["Yemen Ar Rp", ] + Rkbyc["Yemen Dem", ]
Rkbyc <- Rkbyc[!(rownames(Rkbyc) %in% c("Hong Kong", "Taiwan", "South Sudan", 
                                        "Yemen Ar Rp", "Yemen Dem")), ]

# Move pre-1993 Ethiopia to Ethiopia PDR
#Rkbyc <- rbind(Rkbyc, "Ethiopia PDR" = rep(0, ncol(Rkbyc)))
#Rkbyc["Ethiopia PDR", 1:7] <- Rkbyc["Ethiopia", 1:7]
#Rkbyc["Ethiopia", 1:7] <- 0

# Match break points for USSR, Yugoslavia, Czechoslovakia
#match_list <- list(
   # list(former = "USSR", year = 1992, 
    #     new_list = c("Armenia", "Azerbaijan", "Belarus", "Estonia", "Georgia", 
#                  "Kazakhstan", "Kyrgyzstan", "Latvia", "Lithuania", 
#                     "Republic of Moldova", "Russian Federation", "Tajikistan", 
#                     "Turkmenistan", "Ukraine", "Uzbekistan")),
#   list(former = "Yugoslav SFR", year = 1992,
#        new_list = c("Bosnia and Herzegovina", "Croatia", "Serbia and Montenegro", 
#                     "Slovenia", "The former Yugoslav Republic of Macedonia")),
#   list(former = "Czechoslovakia", year = 1993,
#        new_list = c("Czech Republic", "Slovakia"))
#)

#for (l in match_list) {
 #   inew <- which(rownames(Rkbyc) %in% l$new_list)
  #  yrs <- which(as.numeric(colnames(Rkbyc)) < l$year)
   # Rkbyc[l$former, yrs] <- Rkbyc[l$former, yrs] + colSums(Rkbyc[inew, yrs])
#    Rkbyc[inew, yrs] <- 0
#}


## (3) Apportion European Union reserves proportionally to production

eu_list <- c("Austria", "Belgium", "Denmark", "Finland", "France", "Germany",
               "Greece", "Ireland", "Italy", "Luxembourg", "Netherlands", 
               "Portugal", "Spain", "Sweden", "United Kingdom", 
               "Belgium-Luxembourg","Bulgaria", "Croatia", "Cyprus", "Czech Republic",
             "Estonia", "Hungary", "Latvia", "Lithuania", "Malta", "Poland", 
             "Romania", "Slovakia", "Slovenia")
#eu15_yrs <- which(as.numeric(colnames(Rkbyc)) < 1998)
eu_yrs <- which(as.numeric(colnames(Rkbyc)) >= 1998)

# label rows in production matrix from from ISO list
rownames(Pkbyc) <- ciso$name

#prop_eu15 <- scale(Pkbyc[eu15_list, eu15_yrs], center = FALSE,
#                   scale = colSums(Pkbyc[eu15_list, eu15_yrs]))
prop_eu <- scale(Pkbyc[eu_list, eu_yrs], center = FALSE,
                 scale = colSums(Pkbyc[eu_list, eu_yrs]))

# Add individual EU countries to matrix
#Rkbyc <- rbind(Rkbyc, matrix(0, nrow = length(eu15_list), ncol = ncol(Rkbyc), 
 #                            dimnames = list(eu15_list)))
#Rkbyc[eu15_list, eu15_yrs] <- sweep(prop_eu15, 2, Rkbyc["EU-15", eu15_yrs], "*")

Rkbyc <- rbind(Rkbyc, matrix(0, nrow = length(eu_list), ncol = ncol(Rkbyc), 
                                            dimnames = list(eu_list)))
Rkbyc[eu_list, eu_yrs] <- sweep(prop_eu, 2, Rkbyc["European Union", eu_yrs], "*") 
Rkbyc <- Rkbyc[-which(rownames(Rkbyc) %in% c("European Union")), ] #delete EU row


## (4) Create new matrix with all countries in Pkbyc (full country list),
# fill reserves by matching names, leave the rest at zero
Rkbyc_all<-Rkbyc
Rkbyc_all <- Rkbyc_all[match(ciso$name, dimnames(Rkbyc_all)[[1]]),]
Rkbyc_all[is.na(Rkbyc_all)] <- 0
rownames(Rkbyc_all)<-ciso$name
dimnames(Rkbyc_all)<-NULL

#save(Rkbyc_all, file= "cereals_stocks.RData")
saveRDS(Rkbyc_all, file = "inputs/cereals_stocks.RData")

### clear environment
rm(list=ls()) 



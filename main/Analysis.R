### Uses Cascade Functions to calculate network statistics and run various shocks. 

# Must have run all functions in "CascadeFunctions.R" 
# Must have the following in working directory:
#  "prod_trade_file.Rdata" (contains Pkbyc, Tkbyc)
#   stocks file "cereals_stocks.RData"  (Rkbyc_all) 
#   country list - "ciso3.txt" 

setwd("~/Dropbox/MURI/NuclearWinter/FSC-Model/inputs")

library(dplyr)
iso3 <- read.table("ciso3.txt", stringsAsFactors = FALSE) #load country list 
cnames <- iso3[, 2] #select 3 digit ISO character codes 


##Trade data 

years <- c(2005)
trade_dat <- lapply(years, get_trade_data, mov_avg = 1, #load 2005 data with moving average of 1 years on either side
                    prod_trade_file = "cereals_prod_trade.RData", 
                    stocks_file = "cereals_stocks.RData")
names(trade_dat) <- years


#Summary statistics of the trade data for time period considered, 
# $N_c$ = number of countries in network,", $P$ = production, $R$ = reserves, $F$ = trade volume)"))
#"med. year", "$N_c$", "$\\sum P$ (kcal)", "$\\sum R$ (kcal)", "# trade links", "$\\sum F$ (kcal)",  
# "$\\sum R / \\sum P$", "$\\sum F / \\sum P$"
trade_st <- bind_rows(lapply(trade_dat, get_trade_stats_sum), .id = "year")
trade_st

P0<-trade_dat$'2005'$P0
R0<-trade_dat$'2005'$R0
E0<-trade_dat$'2005'$E0


################################
###### ONE SHOCK ###############
################################

#Example 1 - Somalia production decreases by 20% and can access 30% of reserves ##

SomaliaShock<-sim_1c("SOM", 0.20, P0, .30*R0, E0)
# where   c_init = name of the initial country that is impacted
#         a_init = fraction of production lost initially by country c_init
#             P0 = initial production vector
#             R0 = initial reserve vector; note: 0.5*R0 effectively means 50% of reserves can be tapped to absorb shock
#             E0 = initial trade matrix
#OPTIONAL: cfrac = fraction of shock absorbed by C before changing trade (i.e. how much consumption would be reduced)
#           asym = TRUE => shocked countries can't increase their exports
#           kmax = max. number of iterations
#           amin = any shock below this value (as fraction of net supply) is negligible

sim_diagnostics(SomaliaShock)# Tests whether output respects equation: S= P + I - E = R + C

### calculates the depth of the single cascade simulation 
# maximum graph distance from the intital shock country to any country hit by shock
cascade_depth("SOM",SomaliaShock$dE)


##Example 2- USA decreasing 40, can access 50% of reserves
USAShock <- sim_1c("USA", 0.40, P0, 0.50*R0, E0)
sim_diagnostics(USAShock) 
cascade_depth("USA",USAShock$dE)


####################################
#### MULTI-COUNTRY, EQUAL SHOCK ####
###################################

# Example 1 
# 40% decrease in all countries 
AllCountries <- sim_allc(0.4, P0, 0.5*R0, E0, cfrac = 0.01)
# Get a list of summary statistics for sim_res_multi
AllCountriesStats <- get_stats_allc(AllCountries)
AllCountriesStats



#######################################
#### MULTI COUNTRY, VARYING SIZE #####
#######################################
## Simulation of different sized shocks originating at multiple countries

# Example 1 - using wheat anomaly data

# Initalize production shock vector, with percent changes by country
Anomalies<-read.csv("wheat_production_change.csv")
Anomalies$year_1<-Anomalies$year_1/-100 #change percent to prop, decreases=pos, inc=neg
Anomalies$X<-as.character(Anomalies$X)
Anomalies<-Anomalies[!(Anomalies$X=="NULL"),]
Anomalies<-Anomalies[!(Anomalies$X=="MAC"),]
Anomalies<-Anomalies[!(Anomalies$X=="HKG"),]
Anomalies<-Anomalies[!(Anomalies$X=="ARB"),]

shockcountries<-(Anomalies$X)
shockintensities<-(Anomalies$year_1)
names(shockintensities)<-shockcountries
scenarios<-list(shockintensities)

mismatch<-names(shockintensities[!(names(shockintensities)%in%names(trade_dat$`2005`$P0))])
shockintensities<-shockintensities[!(names(shockintensities)%in%mismatch)]
scenarios<-list(shockintensities)


MultiCountry <- sim_mc_multi(scenarios, P0, R0, E0, cfrac = 0.1,
                              asym = T, kmax = 100, amin = 1E-5)
#no equilibrium reached ever.. 

# Get a list of summary statistics for sim_result_mc
MultiCountryStat <- get_stats_mc(MultiCountry) 
MultiCountryStat


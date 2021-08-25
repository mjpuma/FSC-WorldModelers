## Main script for the Food Shock Cascade (FSC) Model

# Load FSC functions
library(dplyr, warn.conflicts = FALSE)
library(tidyr)

# Step 0: Setup ----
# Default 1 year run for static model
years <- 1 #Don't Change
# Create column names for output files
column_names = c('0', '1')
column_names2 = c('1')

# Create output directory if needed
if (dir.exists("outputs") == FALSE) {
  dir.create("outputs")
}

# Step 1: Specify scenario ----
#  Command line : e.g., "rscript main/main.R 0 1 0.5"
# Parse arguments ====
args <- commandArgs(trailingOnly = TRUE)
i_scenario <- c(as.numeric(args[1]))             # Scenario from the FSC Scenario Library
# #  RStudio or similar integrated development environment (IDE)
# #  Specify arguments  ====
# # Specify working directory
# i_scenario <- 34 # See list below 
# setwd("~/GitHub_mjpuma/FSC-WorldModelers/")


# Step 2: Scenario library (read in files) ----
# Production *fractional declines* list by year by country ====
# Read production declines and export restrictions
# NOTE: Currently export restrictions are coded in Step 7 
#       and correspond to the noted files
#  Index based on iso3 alphabetical ordering
#  Scenario name and production decline fractions
if (i_scenario == 1) {
  name_crop <-c('Wheat')
  runname <- c('Wheat_Avg20152017')
  nameinput <- c('Wheat_Avg20152017')
  shock_scenario <- read.csv(paste0("inputs/Scenario1_COVID_WheatDeclineFraction_1Year_195countries.csv"))
  
} else if (i_scenario == 2) {
  name_crop <-c('Maize')
  runname <- c('Maize_Avg20152017')
  nameinput <- c('Maize_Avg20152017')
  shock_scenario <- read.csv(paste0("inputs/Scenario2_COVID_MaizeDeclineFraction_1Year_195countries.csv"))
  
} else if (i_scenario == 3) {
  name_crop <-c('Rice')
  runname <- c('Rice_Avg20152017')
  nameinput <- c('Rice_Avg20152017')
  shock_scenario <- read.csv(paste0("inputs/Scenario3_COVID_RiceDeclineFraction_1Year_195countries.csv"))

# Wheat scenarios (2021) for breadbasket failure and locust declines 
} else if (i_scenario == 11) {
  name_crop <-c('Wheat')
  runname <- c('Wheat_breadbasket_Year2008')
  nameinput <- c('Wheat_Avg20152017')
  shock_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Wheat_DeclineFraction_breadbasket_Year2008_195countries.csv"))
  #export_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))

} else if (i_scenario == 12) {
  name_crop <-c('Wheat')
  runname <- c('Wheat_locust_Year2020')
  nameinput <- c('Wheat_Avg20152017')
  shock_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Wheat_DeclineFraction_locust_Year2020_195countries.csv"))
  #export_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))
  
} else if (i_scenario == 13) {
  name_crop <-c('Wheat')
  runname <- c('Wheat_locust_Year2020_breadbasket_Year2008')
  nameinput <- c('Wheat_Avg20152017')
  shock_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Wheat_DeclineFraction_locust_Year2020_breadbasket_year2008_195countries.csv"))
  #export_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))

} else if (i_scenario == 14) {
  name_crop <-c('Wheat')
  runname <- c('Wheat_ExportRestrictionFraction_Year2008')
  nameinput <- c('Wheat_Avg20152017')
  shock_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))
  #export_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Wheat_ExportRestrictionFraction_Year2008_195countries.csv"))

} else if (i_scenario == 15) {
  name_crop <-c('Wheat')
  runname <- c('Wheat_ExportRestrictionFraction_Year2008with_RUS_25')
  nameinput <- c('Wheat_Avg20152017')
  shock_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))
  #export_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Wheat_ExportRestrictionFraction_Year2008_with_RUS_25%_195countries.csv"))

} else if (i_scenario == 16) {
  name_crop <-c('Wheat')
  runname <- c('Wheat_ExportRestrictionFraction_Year2008with_RUS_50')
  nameinput <- c('Wheat_Avg20152017')
  shock_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))
  #export_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Wheat_ExportRestrictionFraction_Year2008_with_RUS_50%_195countries.csv"))

# Maize scenarios (2021) for breadbasket failure and locust declines 
} else if (i_scenario == 21) {
  name_crop <-c('Maize')
  runname <- c('Maize_breadbasket_Year2008')
  nameinput <- c('Maize_Avg20152017')
  shock_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Maize_DeclineFraction_breadbasket_Year2008_195countries.csv"))
  #export_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))

} else if (i_scenario == 22) {
  name_crop <-c('Maize')
  runname <- c('Maize_locust_Year2020')
  nameinput <- c('Maize_Avg20152017')
  shock_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Maize_DeclineFraction_locust_Year2020_195countries.csv"))
  #export_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))
  
} else if (i_scenario == 23) {
  name_crop <-c('Maize')
  runname <- c('Maize_locust_Year2020_breadbasket_Year2008')
  nameinput <- c('Maize_Avg20152017')
  shock_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Maize_DeclineFraction_locust_Year2020_breadbasket_year2008_195countries.csv"))
  #export_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))

} else if (i_scenario == 24) {
  name_crop <-c('Maize')
  runname <- c('Maize_ExportRestrictionFraction_Year2008')
  nameinput <- c('Maize_Avg20152017')
  shock_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))
  #export_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Maize_ExportRestrictionFraction_Year2008_195countries.csv"))

} else if (i_scenario == 25) {
  name_crop <-c('Maize')
  runname <- c('Maize_ExportRestrictionFraction_Year2008with_ARG_25')
  nameinput <- c('Maize_Avg20152017')
  shock_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))
  #export_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Maize_ExportRestrictionFraction_Year2008_with_ARG_25%_195countries.csv"))

} else if (i_scenario == 26) {
  name_crop <-c('Maize')
  runname <- c('Maize_ExportRestrictionFraction_Year2008with_ARG_50')
  nameinput <- c('Maize_Avg20152017')
  shock_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))
  #export_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Maize_ExportRestrictionFraction_Year2008_with_ARG_50%_195countries.csv"))

# Rice scenarios (2021) for breadbasket failure and locust declines 
} else if (i_scenario == 31) {
  name_crop <-c('Rice')
  runname <- c('Rice_breadbasket_Year2008')
  nameinput <- c('Rice_Avg20152017')
  shock_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Rice_DeclineFraction_breadbasket_Year2008_195countries.csv"))
  #export_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))

} else if (i_scenario == 32) {
  name_crop <-c('Rice')
  runname <- c('Rice_locust_Year2020')
  nameinput <- c('Rice_Avg20152017')
  shock_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Rice_DeclineFraction_locust_Year2020_195countries.csv"))
  #export_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))
  
} else if (i_scenario == 33) {
  name_crop <-c('Rice')
  runname <- c('Rice_locust_Year2020_breadbasket_Year2008')
  nameinput <- c('Rice_Avg20152017')
  shock_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Rice_DeclineFraction_locust_Year2020_breadbasket_year2008_195countries.csv"))
  #export_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))

} else if (i_scenario == 34) {
  name_crop <-c('Rice')
  runname <- c('Rice_ExportRestrictionFraction_Year2008')
  nameinput <- c('Rice_Avg20152017')
  shock_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))
  #export_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Rice_ExportRestrictionFraction_Year2008_195countries.csv"))

}


# Step 3: Load ancillary data ----
# i) Commodity list for bilateral trade
commodities <- read.csv(paste0("ancillary/", nameinput, "cropcommodity_tradelist.csv"))
# ii) Load country list
country_list <- read.csv("ancillary/country_list195_2012to2016.csv")
country_list <- country_list[order(country_list$iso3), ] # Order by iso3 code


# Step 4: Load production/trade/stocks data ----
load(paste0("inputs_processed/", nameinput, "E0.RData")) #Export Matrix ordered by FAOSTAT country code (increasing)
load(paste0("inputs_processed/", nameinput, "P0.Rdata")) #Production
load(paste0("inputs_processed/", nameinput, "R0.RData")) #Reserves (a.k.a. Stocks)


# Step 5: Setup production and shocks; initialize output vectors ----
# Assign production vector to P0 ====
P0 <- Pkbyc

# Create 'Shocks' dataframe ====
Shocks <- merge(country_list,shock_scenario,by = 'iso3')
Shocks[is.na(Shocks)] <- 0

# Order shocks dataframe by FAOSTAT country code (in increasing order) ====
P <- P0
Prod <- as.numeric(unlist(P0$P0))
Shocks <- merge(Shocks, P, by = "iso3")
Shocks <- Shocks[order(Shocks$iso3), ]

# # Create Export Restriction dataframe ====
# ExportRestrict <- merge(country_list,export_scenario,by = 'iso3')
# ExportRestrict[is.na(ExportRestrict)] <- 0

# # Order ExportRestrict dataframe by FAOSTAT country code (in increasing order) ====
# ExportRestrict <- merge(ExportRestrict, P, by = "iso3")
# ExportRestrict <- ExportRestrict[order(ExportRestrict$iso3), ]

# Initialize  output vectors ====
Pout <-  array(0, c(nrow(country_list), length(years) + 1))
Rout <-  array(0, c(nrow(country_list), length(years) + 1))
Cout <-  array(0, c(nrow(country_list), length(years) + 1))
Eout <-  array(0, c(nrow(country_list), nrow(country_list), length(years) + 1))

shortageout <- array(0, c(nrow(country_list), length(years)))
C1_C0out <- array(0, c(nrow(country_list), length(years)))
C2_C0out <- array(0, c(nrow(country_list), length(years)))
dR_C0out <- array(0, c(nrow(country_list), length(years)))

## Add initial conditions to output arrays
E0 <- E0_avg
Eout[, , 1] <- E0
Pout[, 1] <- Prod
Rout[, 1] <- R0

## Create 'InputFSC' dataframe adding initial reserves====
InputFSC <- data_frame(iso3 = names(R0), R0 = R0)

# Step 6:
# Separate NEGATIVE and POSITIVE shock anomalies ====
#   Fractional gains and losses in production
#   Note: Initial production, P0, is fixed but Shocks vary in time
FracGain <- Shocks$year_1
FracGain[FracGain > 0] <- 0
FracGain <- -FracGain      # adjust sign (fractional *declines* read in)

FracLoss <- Shocks[4]
FracLoss[FracLoss < 0] <- 0
FracLoss <- -FracLoss      # adjust sign (fractional *declines* read in)

# Create vector for NEGATIVE shock anomalies ====
dP <-  FracLoss * Shocks$P0 
Shocks$dP <- dP

# Set Reserves and add POSTIVE anomalies to reserves ====
InputFSC <- merge(Shocks,InputFSC, by = 'iso3', all.x = TRUE, all.y = FALSE)
InputFSC <- InputFSC[order(InputFSC$iso3), ]

# Add positive anomalies to reserves ====
InputFSC$Rcurrent <- InputFSC$R0  + (InputFSC$P0 * FracGain)
Rcurrent <- as.numeric(unlist(InputFSC$Rcurrent))

# Update state variables for trade_dat ====
# Assign production, reserves, and export matrix
trade_dat <- list(P = Prod, R = Rcurrent, E = E0)
# Number of countries
trade_dat$nc <- length(trade_dat$P)
# Change in reserves; set initially to zero
trade_dat$dR <- rep(0, trade_dat$nc)
# Compute consumption assuming that it is initially equal to supply
trade_dat$C <- trade_dat$P + colSums(trade_dat$E) - rowSums(trade_dat$E) 
C0_initial <- trade_dat$C
Cout[, 1] <- C0_initial
# Initial shortage is 0
trade_dat$shortage <- rep(0, trade_dat$nc)

# Domestic Supply after production declines
trade_dat$C1 <- trade_dat$P + Shocks$dP + colSums(trade_dat$E) - rowSums(trade_dat$E)

ImportsInitial<-colSums(trade_dat$E)
ExportsInitial<-rowSums(trade_dat$E)

# Step 7:
# Specify and impose export restrictions for COVID-19 scenario ----
if (i_scenario == 1) {
  # Wheat
  i_Russia = which(row.names(E0)=='RUS')   
  i_Ukraine = which(row.names(E0)=='UKR')  
  i_Kazakhstan = which(row.names(E0)=='KAZ') 
  
  #   Add *restricted* exports to reserves
  Rcurrent[i_Russia] = Rcurrent[i_Russia] + sum(E0[i_Russia,])
  Rcurrent[i_Ukraine] = Rcurrent[i_Ukraine] + sum(E0[i_Ukraine,])
  Rcurrent[i_Kazakhstan] = Rcurrent[i_Kazakhstan] + sum(E0[i_Kazakhstan,])
  
  #  Impose export restrictions by setting export values to zero
  E0[i_Russia,]<-0
  E0[i_Ukraine,]<-0
  E0[i_Kazakhstan,]<-0
  
} else if (i_scenario == 2) {
  # Rice
  i_India = which(row.names(E0)=='IND')    # country_list[76,] India
  i_Thailand = which(row.names(E0)=='THA')   # country_list[167,] Thailand
  i_Vietnam = which(row.names(E0)=='VNM') # country_list[184,]  Vietnam
  
  #   Add *restricted* exports to reserves
  Rcurrent[i_India] = Rcurrent[i_India] + sum(E0[i_India,])
  Rcurrent[i_Thailand] = Rcurrent[i_Thailand] + sum(E0[i_Thailand,])
  Rcurrent[i_Vietnam] = Rcurrent[i_Vietnam] + sum(E0[i_Vietnam,])
  
  #  Impose export restrictions by setting export values to zero
  E0[i_India,]<-0
  E0[i_Thailand,]<-0
  E0[i_Vietnam,]<-0
  
} else if (i_scenario == 3) {
  # Maize
  i_Argentina = which(row.names(E0)=='ARG')    # country_list[6,] Argentina
  i_Brazil = which(row.names(E0)=='BRA')   # country_list[17,] Brazil
  i_Ukraine = which(row.names(E0)=='UKR') # country_list[178,]  Ukraine
  
  #   Add *restricted* exports to reserves
  Rcurrent[i_Argentina] = Rcurrent[i_Argentina] + sum(E0[i_Argentina,])
  Rcurrent[i_Brazil] = Rcurrent[i_Brazil] + sum(E0[i_Brazil,])
  Rcurrent[i_Ukraine] = Rcurrent[i_Ukraine] + sum(E0[i_Ukraine,])
  
  #  Impose export restrictions by setting export values to zero
  E0[i_Argentina,]<-0
  E0[i_Brazil,]<-0
  E0[i_Ukraine,]<-0

# Wheat scenarios (2021) for breadbasket failure and locust declines ----
} else if (i_scenario == 14) {
  # export_scenario: Scenario_Wheat_ExportRestrictionFraction_Year2008_195countries.csv
     # ARG	1
     i_restrict = which(row.names(E0)=='ARG')
     reduce_factor <- 1
     #   Add *restricted* exports to reserves
     Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
     #  Impose export restrictions by reducing exports
     E0[i_restrict,]<- reduce_factor*E0[i_restrict,]

     #GIN	1
     i_restrict = which(row.names(E0)=='GIN')
     reduce_factor <- 1
     #   Add *restricted* exports to reserves
     Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
     #  Impose export restrictions by reducing exports
     E0[i_restrict,]<- reduce_factor*E0[i_restrict,]

     #IND	1
     i_restrict = which(row.names(E0)=='IND')
     reduce_factor <- 1
     #   Add *restricted* exports to reserves
     Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
     #  Impose export restrictions by reducing exports
     E0[i_restrict,]<- reduce_factor*E0[i_restrict,]

     # PAK	1
     i_restrict = which(row.names(E0)=='PAK')
     reduce_factor <- 1
     #   Add *restricted* exports to reserves
     Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
     #  Impose export restrictions by reducing exports
     E0[i_restrict,]<- reduce_factor*E0[i_restrict,]
     
     # ETH	1
     i_restrict = which(row.names(E0)=='PAK')
     reduce_factor <- 1
     #   Add *restricted* exports to reserves
     Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
     #  Impose export restrictions by reducing exports
     E0[i_restrict,]<- reduce_factor*E0[i_restrict,]
     
     # SYR	0.92
     i_restrict = which(row.names(E0)=='SYR')
     reduce_factor <- 0.92
     #   Add *restricted* exports to reserves
     Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
     #  Impose export restrictions by reducing exports
     E0[i_restrict,]<- reduce_factor*E0[i_restrict,]
     
     # NPL	0.33
     i_restrict = which(row.names(E0)=='NPL')
     reduce_factor <- 0.33
     #   Add *restricted* exports to reserves
     Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
     #  Impose export restrictions by reducing exports
     E0[i_restrict,]<- reduce_factor*E0[i_restrict,]

     # KAZ	0.17
     i_restrict = which(row.names(E0)=='KAZ')
     reduce_factor <- 0.17
     #   Add *restricted* exports to reserves
     Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
     #  Impose export restrictions by reducing exports
     E0[i_restrict,]<- reduce_factor*E0[i_restrict,]

     # BOL	0.13
     i_restrict = which(row.names(E0)=='BOL')
     reduce_factor <- 0.13
     #   Add *restricted* exports to reserves
     Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
     #  Impose export restrictions by reducing exports
     E0[i_restrict,]<- reduce_factor*E0[i_restrict,]

     } else if (i_scenario == 15) {
  # export_scenario: Scenario_Wheat_ExportRestrictionFraction_Year2008_with_RUS_25%_195countries.csv
     # RUS 0.25
     i_restrict = which(row.names(E0)=='RUS')
     reduce_factor <- 0.25
     #   Add *restricted* exports to reserves
     Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
     #  Impose export restrictions by reducing exports
     E0[i_restrict,]<- reduce_factor*E0[i_restrict,]

     # ARG	1
     i_restrict = which(row.names(E0)=='ARG')
     reduce_factor <- 1
     #   Add *restricted* exports to reserves
     Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
     #  Impose export restrictions by reducing exports
     E0[i_restrict,]<- reduce_factor*E0[i_restrict,]

     #GIN	1
     i_restrict = which(row.names(E0)=='GIN')
     reduce_factor <- 1
     #   Add *restricted* exports to reserves
     Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
     #  Impose export restrictions by reducing exports
     E0[i_restrict,]<- reduce_factor*E0[i_restrict,]

     #IND	1
     i_restrict = which(row.names(E0)=='IND')
     reduce_factor <- 1
     #   Add *restricted* exports to reserves
     Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
     #  Impose export restrictions by reducing exports
     E0[i_restrict,]<- reduce_factor*E0[i_restrict,]

     # PAK	1
     i_restrict = which(row.names(E0)=='PAK')
     reduce_factor <- 1
     #   Add *restricted* exports to reserves
     Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
     #  Impose export restrictions by reducing exports
     E0[i_restrict,]<- reduce_factor*E0[i_restrict,]
     
     # ETH	1
     i_restrict = which(row.names(E0)=='PAK')
     reduce_factor <- 1
     #   Add *restricted* exports to reserves
     Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
     #  Impose export restrictions by reducing exports
     E0[i_restrict,]<- reduce_factor*E0[i_restrict,]
     
     # SYR	0.92
     i_restrict = which(row.names(E0)=='SYR')
     reduce_factor <- 0.92
     #   Add *restricted* exports to reserves
     Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
     #  Impose export restrictions by reducing exports
     E0[i_restrict,]<- reduce_factor*E0[i_restrict,]
     
     # NPL	0.33
     i_restrict = which(row.names(E0)=='NPL')
     reduce_factor <- 0.33
     #   Add *restricted* exports to reserves
     Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
     #  Impose export restrictions by reducing exports
     E0[i_restrict,]<- reduce_factor*E0[i_restrict,]

     # KAZ	0.17
     i_restrict = which(row.names(E0)=='KAZ')
     reduce_factor <- 0.17
     #   Add *restricted* exports to reserves
     Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
     #  Impose export restrictions by reducing exports
     E0[i_restrict,]<- reduce_factor*E0[i_restrict,]

     # BOL	0.13
     i_restrict = which(row.names(E0)=='BOL')
     reduce_factor <- 0.13
     #   Add *restricted* exports to reserves
     Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
     #  Impose export restrictions by reducing exports
     E0[i_restrict,]<- reduce_factor*E0[i_restrict,]

     } else if (i_scenario == 16) {
  # export_scenario: Scenario_Wheat_ExportRestrictionFraction_Year2008_with_RUS_50%_195countries.csv
     # RUS 0.5
     i_restrict = which(row.names(E0)=='RUS')
     reduce_factor <- 0.5
     #   Add *restricted* exports to reserves
     Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
     #  Impose export restrictions by reducing exports
     E0[i_restrict,]<- reduce_factor*E0[i_restrict,]

     # ARG	1
     i_restrict = which(row.names(E0)=='ARG')
     reduce_factor <- 1
     #   Add *restricted* exports to reserves
     Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
     #  Impose export restrictions by reducing exports
     E0[i_restrict,]<- reduce_factor*E0[i_restrict,]

     #GIN	1
     i_restrict = which(row.names(E0)=='GIN')
     reduce_factor <- 1
     #   Add *restricted* exports to reserves
     Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
     #  Impose export restrictions by reducing exports
     E0[i_restrict,]<- reduce_factor*E0[i_restrict,]

     #IND	1
     i_restrict = which(row.names(E0)=='IND')
     reduce_factor <- 1
     #   Add *restricted* exports to reserves
     Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
     #  Impose export restrictions by reducing exports
     E0[i_restrict,]<- reduce_factor*E0[i_restrict,]

     # PAK	1
     i_restrict = which(row.names(E0)=='PAK')
     reduce_factor <- 1
     #   Add *restricted* exports to reserves
     Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
     #  Impose export restrictions by reducing exports
     E0[i_restrict,]<- reduce_factor*E0[i_restrict,]
     
     # ETH	1
     i_restrict = which(row.names(E0)=='PAK')
     reduce_factor <- 1
     #   Add *restricted* exports to reserves
     Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
     #  Impose export restrictions by reducing exports
     E0[i_restrict,]<- reduce_factor*E0[i_restrict,]
     
     # SYR	0.92
     i_restrict = which(row.names(E0)=='SYR')
     reduce_factor <- 0.92
     #   Add *restricted* exports to reserves
     Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
     #  Impose export restrictions by reducing exports
     E0[i_restrict,]<- reduce_factor*E0[i_restrict,]
     
     # NPL	0.33
     i_restrict = which(row.names(E0)=='NPL')
     reduce_factor <- 0.33
     #   Add *restricted* exports to reserves
     Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
     #  Impose export restrictions by reducing exports
     E0[i_restrict,]<- reduce_factor*E0[i_restrict,]

     # KAZ	0.17
     i_restrict = which(row.names(E0)=='KAZ')
     reduce_factor <- 0.17
     #   Add *restricted* exports to reserves
     Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
     #  Impose export restrictions by reducing exports
     E0[i_restrict,]<- reduce_factor*E0[i_restrict,]

     # BOL	0.13
     i_restrict = which(row.names(E0)=='BOL')
     reduce_factor <- 0.13
     #   Add *restricted* exports to reserves
     Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
     #  Impose export restrictions by reducing exports
     E0[i_restrict,]<- reduce_factor*E0[i_restrict,]


# Maize scenarios (2021) for breadbasket failure and locust declines
  } else if (i_scenario == 24) {
    # export_scenario: Scenario_Maize_ExportRestrictionFraction_Year2008_195countries.csv
    # KEN	1
    i_restrict = which(row.names(E0)=='KEN')
    reduce_factor <- 1
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- reduce_factor*E0[i_restrict,]
    
    # ETH	1
    i_restrict = which(row.names(E0)=='ETH')
    reduce_factor <- 1
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- reduce_factor*E0[i_restrict,]
    
    # ZMB	1
     i_restrict = which(row.names(E0)=='ZMB')
    reduce_factor <- 1
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- reduce_factor*E0[i_restrict,]   
    
    # GIN	0.5
    i_restrict = which(row.names(E0)=='GIN')
    reduce_factor <- 0.5
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- reduce_factor*E0[i_restrict,]    
    
    # CHN	0.25
    i_restrict = which(row.names(E0)=='CHN')
    reduce_factor <- 0.25
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- reduce_factor*E0[i_restrict,]    
    
    # NPL	0.08
    i_restrict = which(row.names(E0)=='NPL')
    reduce_factor <- 0.08
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- reduce_factor*E0[i_restrict,]

  } else if (i_scenario == 25) {
  # export_scenario: Scenario_Maize_ExportRestrictionFraction_Year2008_with_ARG_25%_195countries.csv
    i_restrict = which(row.names(E0)=='ARG')
    reduce_factor <- 0.25
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- reduce_factor*E0[i_restrict,]

    # Export_scenario: Scenario_Maize_ExportRestrictionFraction_Year2008_195countries.csv
    # KEN	1
    i_restrict = which(row.names(E0)=='KEN')
    reduce_factor <- 1
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- reduce_factor*E0[i_restrict,]
    
    # ETH	1
    i_restrict = which(row.names(E0)=='ETH')
    reduce_factor <- 1
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- reduce_factor*E0[i_restrict,]
    
    # ZMB	1
     i_restrict = which(row.names(E0)=='ZMB')
    reduce_factor <- 1
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- reduce_factor*E0[i_restrict,]   
    
    # GIN	0.5
    i_restrict = which(row.names(E0)=='GIN')
    reduce_factor <- 0.5
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- reduce_factor*E0[i_restrict,]    
    
    # CHN	0.25
    i_restrict = which(row.names(E0)=='CHN')
    reduce_factor <- 0.25
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- reduce_factor*E0[i_restrict,]    
    
    # NPL	0.08
    i_restrict = which(row.names(E0)=='NPL')
    reduce_factor <- 0.08
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- reduce_factor*E0[i_restrict,]

  } else if (i_scenario == 26) {
  # export_scenario: Scenario_Maize_ExportRestrictionFraction_Year2008_with_ARG_50%_195countries.csv
    i_restrict = which(row.names(E0)=='ARG')
    reduce_factor <- 0.50
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- reduce_factor*E0[i_restrict,]

    # Export_scenario: Scenario_Maize_ExportRestrictionFraction_Year2008_195countries.csv
    # KEN	1
    i_restrict = which(row.names(E0)=='KEN')
    reduce_factor <- 1
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- reduce_factor*E0[i_restrict,]
    
    # ETH	1
    i_restrict = which(row.names(E0)=='ETH')
    reduce_factor <- 1
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- reduce_factor*E0[i_restrict,]
    
    # ZMB	1
     i_restrict = which(row.names(E0)=='ZMB')
    reduce_factor <- 1
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- reduce_factor*E0[i_restrict,]   
    
    # GIN	0.5
    i_restrict = which(row.names(E0)=='GIN')
    reduce_factor <- 0.5
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- reduce_factor*E0[i_restrict,]    
    
    # CHN	0.25
    i_restrict = which(row.names(E0)=='CHN')
    reduce_factor <- 0.25
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- reduce_factor*E0[i_restrict,]    
    
    # NPL	0.08
    i_restrict = which(row.names(E0)=='NPL')
    reduce_factor <- 0.08
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- reduce_factor*E0[i_restrict,]

} else if (i_scenario == 34) {
  # export_scenario: Scenario_Rice_ExportRestrictionFraction_Year2008_195countries.csv
    # CHN	1
    i_restrict = which(row.names(E0)=='CHN')
    reduce_factor <- 1
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- reduce_factor*E0[i_restrict,]
    
    # ECU	1
    i_restrict = which(row.names(E0)=='ECU')
    reduce_factor <- 1
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- reduce_factor*E0[i_restrict,]

    # IDN	1
    i_restrict = which(row.names(E0)=='IDN')
    reduce_factor <- 1
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- reduce_factor*E0[i_restrict,]

    # VNM	1
        i_restrict = which(row.names(E0)=='VNM')
    reduce_factor <- 1
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- reduce_factor*E0[i_restrict,]

    # ETH	1
    i_restrict = which(row.names(E0)=='ETH')
    reduce_factor <- 1
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- reduce_factor*E0[i_restrict,]

    # BRA	0.83
        i_restrict = which(row.names(E0)=='BRA')
    reduce_factor <- 0.83
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- reduce_factor*E0[i_restrict,]

    # IND	0.83
        i_restrict = which(row.names(E0)=='IND')
    reduce_factor <- 0.83
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- reduce_factor*E0[i_restrict,]

    # KHM	0.83
    i_restrict = which(row.names(E0)=='KHM')
    reduce_factor <- 0.83
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- reduce_factor*E0[i_restrict,]
    
    # ARG	0.75
    i_restrict = which(row.names(E0)=='ARG')
    reduce_factor <- 0.75
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- reduce_factor*E0[i_restrict,]

    # GIN	0.75
    i_restrict = which(row.names(E0)=='GIN')
    reduce_factor <- 0.75
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- reduce_factor*E0[i_restrict,]

    # TZA	0.67
    i_restrict = which(row.names(E0)=='TZA')
    reduce_factor <- 0.67
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- reduce_factor*E0[i_restrict,]
    
    # THA	0.67
    i_restrict = which(row.names(E0)=='THA')
    reduce_factor <- 0.67
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- reduce_factor*E0[i_restrict,]
    
    # MDG	0.63
    i_restrict = which(row.names(E0)=='MDG')
    reduce_factor <- 0.63
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- reduce_factor*E0[i_restrict,]
    
    # MMR	0.5
    i_restrict = which(row.names(E0)=='MMR')
    reduce_factor <- 0.5
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- reduce_factor*E0[i_restrict,]
    
    # EGY	0.5
    i_restrict = which(row.names(E0)=='EGY')
    reduce_factor <- 0.5
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- reduce_factor*E0[i_restrict,]
    
    # NPL	0.5
    i_restrict = which(row.names(E0)=='NPL')
    reduce_factor <- 0.5
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- reduce_factor*E0[i_restrict,]
    
    # PAK	0.5
    i_restrict = which(row.names(E0)=='PAK')
    reduce_factor <- 0.5
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- reduce_factor*E0[i_restrict,]
    
    # BGD	0.17
    i_restrict = which(row.names(E0)=='BGD')
    reduce_factor <- 0.17
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- reduce_factor*E0[i_restrict,]
}

# Step 8: Update food balance variables ----
#  Update state variables for trade_dat after trade restrictions
trade_dat$R <- Rcurrent
trade_dat$E <- E0

# Domestic Supply after production declines and trade restrictions
trade_dat$C2 <- trade_dat$P + Shocks$dP + colSums(trade_dat$E) - rowSums(trade_dat$E)

ImportsFinal<-colSums(trade_dat$E)
ExportsFinal<-rowSums(trade_dat$E)

#Update results
results_FSCstatic <- list(P = trade_dat$P + Shocks$dP, R = Rcurrent, C1=trade_dat$C1, C2=trade_dat$C2, shortage =trade_dat$shortage,E=E0)

# Store outputs of interest from simulations ====
#   Output: 1D arrays
i=1
Pout[, i+1]    <- as.numeric(unlist(results_FSCstatic$P))
Rout[, i+1]    <- as.numeric(unlist(results_FSCstatic$R))
Cout[, i+1]    <- as.numeric(unlist(results_FSCstatic$C1))
shortageout[, i] <- as.numeric(unlist(results_FSCstatic$shortage))
# Consumption relative to initial consumption
C1_C0out[, i] <- as.numeric(unlist(results_FSCstatic$C1)) / C0_initial
C2_C0out[, i] <- as.numeric(unlist(results_FSCstatic$C2)) / C0_initial
#     change in reserves relative to initial consumption
#dR_C0out[, i] <- results_FSC$dR / C0_initial
  
# Step 9: Collect, reformat and save output data ----
# Production
colnames(Pout)  <- column_names
rownames(Pout)  <- InputFSC$iso3
Pout_df <- data.frame(Pout)
Pout_df <- tibble::rownames_to_column(Pout_df, "iso3")
Pout_df <- merge(InputFSC[, c("iso3", "Country.x")], Pout_df, by = "iso3")
# combine the year columns into a single column with separate rows for each year; assign to new vector
Pout_df <- gather(Pout_df, Year, Value, -iso3, -Country.x)
# remove preceeding X character for Year column aand convert to numeric
Pout_df$Year <- as.numeric(gsub("[a-zA-Z ]", "", Pout_df$Year))

# Reserves
colnames(Rout)  <- column_names
rownames(Rout)  <- InputFSC$iso3
Rout_df <- data.frame(Rout)
Rout_df <- tibble::rownames_to_column(Rout_df, "iso3")
Rout_df <- merge(InputFSC[, c("iso3", "Country.x")], Rout_df, by = "iso3")
# combine the year columns into a single column with separate rows for each year; assign to new vector
Rout_df <- gather(Rout_df, Year, Value, -iso3, -Country.x)
# remove preceeding X character for Year column aand convert to numeric
Rout_df$Year <- as.numeric(gsub("[a-zA-Z ]", "", Rout_df$Year))

# Trade matrix (2D array)
Eout[, , i+1]  <- as.numeric(unlist(results_FSCstatic$E))
colnames(Eout)  <- InputFSC$iso3
rownames(Eout)  <- InputFSC$iso3

# Shortage
colnames(shortageout)  <- column_names2
rownames(shortageout)  <- InputFSC$iso3
shortageout_df <- data.frame(shortageout)
shortageout_df <- tibble::rownames_to_column(shortageout_df, "iso3")
shortageout_df <-
  merge(InputFSC[, c("iso3", "Country.x")], shortageout_df, by = "iso3")
# combine the year columns into a single column with separate rows for each year; assign to new vector
shortageout_df <- gather(shortageout_df, Year, Value, -iso3, -Country.x)
# remove preceeding X character for Year column aand convert to numeric
shortageout_df$Year <-
  as.numeric(gsub("[a-zA-Z ]", "", shortageout_df$Year))

# Consumption to C0
colnames(C1_C0out)  <- column_names2
rownames(C1_C0out)  <- InputFSC$iso3
C1_C0out_df <- data.frame(C1_C0out)
C1_C0out_df <- tibble::rownames_to_column(C1_C0out_df, "iso3")
C1_C0out_df <-
  merge(InputFSC[, c("iso3", "Country.x")], C1_C0out_df, by = "iso3")
# combine the year columns into a single column with separate rows for each year; assign to new vector
C1_C0out_df <- gather(C1_C0out_df, Year, Value, -iso3, -Country.x)
# remove preceeding X character for Year column aand convert to numeric
C1_C0out_df$Year <- as.numeric(gsub("[a-zA-Z ]", "", C1_C0out_df$Year))

colnames(C2_C0out)  <- column_names2
rownames(C2_C0out)  <- InputFSC$iso3
C2_C0out_df <- data.frame(C2_C0out)
C2_C0out_df <- tibble::rownames_to_column(C2_C0out_df, "iso3")
C2_C0out_df <-
  merge(InputFSC[, c("iso3", "Country.x")], C2_C0out_df, by = "iso3")
# combine the year columns into a single column with separate rows for each year; assign to new vector
C2_C0out_df <- gather(C2_C0out_df, Year, Value, -iso3, -Country.x)
# remove preceeding X character for Year column and convert to numeric
C2_C0out_df$Year <- as.numeric(gsub("[a-zA-Z ]", "", C2_C0out_df$Year))

## Save as CSV
write.csv(Pout_df, paste0("outputs/",runname,"ProductionStatic.csv"), row.names = FALSE)
write.csv(Rout_df, paste0("outputs/",runname,"ReserveStatic.csv"), row.names = FALSE)
write.csv(shortageout_df, paste0("outputs/",runname,"ShortageStatic.csv"), row.names = FALSE)
write.csv(C1_C0out_df, paste0("outputs/",runname,"C1C0Static.csv"), row.names = FALSE)
write.csv(C2_C0out_df, paste0("outputs/",runname,"C2C0Static.csv"), row.names = FALSE)
write.csv(Eout, paste0("outputs/",runname, "ExportStatic.csv"), row.names = TRUE)

## Save lists of initial and final imports and outputs by country
write.csv(ExportsFinal, paste0("outputs/",runname, "ExportsFinal.csv"), row.names = FALSE)
write.csv(ExportsInitial, paste0("outputs/",runname, "ExportsInitial.csv"), row.names = FALSE)
write.csv(ImportsFinal, paste0("outputs/",runname, "ImportsFinal.csv"), row.names = FALSE)
write.csv(ImportsInitial, paste0("outputs/",runname, "ImportsInitial.csv"), row.names = FALSE)

# Save Exports as R data file
saveRDS(Eout, file = paste0("outputs/", runname,"ExportStatic.rds"))

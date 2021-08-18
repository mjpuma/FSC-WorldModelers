## Main script for the Food Shock Cascade (FSC) Model

# Create output directory if needed
if (dir.exists("outputs") == FALSE) {
  dir.create("outputs")
}

# Step 0: Load FSC functions ----
library(dplyr, warn.conflicts = FALSE)
library(tidyr)

# Step 1: Input Arguments ----
# RStudio version: Specify arguments  ====
setwd("~/GitHub_mjpuma/FSC-WorldModelers/")
years <- 1 # Set year range to run model
# Create column names for output files
column_names = c('0', '1')
column_names2 = c('1')

# Step 2: Specify crop + exogenous trade restriction scenario ----
i_scenario = 1 # See list below 

# Production *fractional declines* list by year by country ====
# Read production declines and Select countries for export bans
#  Index based on iso3 alphabetical ordering
#  Scenario name and production decline fractions
if (i_scenario == 1) {
  name_crop <-c('Wheat')
  runname <- c('Wheat_Avg20152017')
  shock_scenario <- read.csv(paste0("inputs/Scenario1_COVID_WheatDeclineFraction_1Year_195countries.csv"))
  
} else if (i_scenario == 2) {
  name_crop <-c('Maize')
  runname <- c('Maize_Avg20152017')
  shock_scenario <- read.csv(paste0("inputs/Scenario2_COVID_MaizeDeclineFraction_1Year_195countries.csv"))
  
} else if (i_scenario == 3) {
  name_crop <-c('Rice')
  runname <- c('Rice_Avg20152017')
  shock_scenario <- read.csv(paste0("inputs/Scenario3_COVID_RiceDeclineFraction_1Year_195countries.csv"))
  
} else if (i_scenario == 4) {
  name_crop <-c('Wheat')
  runname <- c('Wheat_breadbasket_Year2008')
  shock_scenario <- read.csv(paste0("inputs/Scenario_Wheat_DeclineFraction_breadbasket_Year2008_195countries.csv"))

} else if (i_scenario == 5) {
  name_crop <-c('Wheat')
  runname <- c('Wheat_locust_Year2020')
  shock_scenario <- read.csv(paste0("inputs/Scenario_Wheat_DeclineFraction_locust_Year2020_195countries.csv"))
  
} else if (i_scenario == 6) {
  name_crop <-c('Wheat')
  runname <- c('Wheat_locust_Year2020_breadbasket_Year2008')
  shock_scenario <- read.csv(paste0("inputs/Scenario_Wheat_DeclineFraction_locust_Year2020_breadbasket_year2008_195countries.csv"))
}

# Step 3: Load ancillary data ----
# i) Commodity list for bilateral trade
commodities <- read.csv(paste0("ancillary/", runname, "cropcommodity_tradelist.csv"))
# ii) Load country list
country_list <- read.csv("ancillary/country_list195_2012to2016.csv")
country_list <- country_list[order(country_list$iso3), ] # Order by iso3 code


# Step 4: Load production/trade/stocks data ----
load(paste0("inputs_processed/", runname, "E0.RData")) #Export Matrix ordered by FAOSTAT country code (increasing)
load(paste0("inputs_processed/", runname, "P0.Rdata")) #Production
load(paste0("inputs_processed/", runname, "R0.RData")) #Reserves (a.k.a. Stocks)


# Step 5: Setup production and shocks; initialize output vectors ----
# Assign production vector to P0 ====
P0 <- Pkbyc

# Create 'Shocks' dataframe ====
Shocks <- merge(country_list,shock_scenario,by = 'iso3')
Shocks[is.na(Shocks)] <- 0

# Order shocks dataframe by FAOSTAT country code (in increasing order) ====
P <- P0
Shocks <- merge(Shocks, P, by = "iso3")
Shocks <- Shocks[order(Shocks$iso3), ]
Prod <- as.numeric(unlist(P0$P0))

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

# Impose export restrictions for COVID-19 scenario
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
}

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
  
# Step 7: Collect, reformat and save output data ----
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
write.csv(Pout_df, paste0("outputs/data_network/",runname,"ProductionStatic.csv"), row.names = FALSE)
write.csv(Rout_df, paste0("outputs/data_network/",runname,"ReserveStatic.csv"), row.names = FALSE)
write.csv(shortageout_df, paste0("outputs/data_network/",runname,"ShortageStatic.csv"), row.names = FALSE)
write.csv(C1_C0out_df, paste0("outputs/data_network/",runname,"C1C0Static.csv"), row.names = FALSE)
write.csv(C2_C0out_df, paste0("outputs/data_network/",runname,"C2C0Static.csv"), row.names = FALSE)
write.csv(Eout, paste0("outputs/data_network/",runname, "ExportStatic.csv"), row.names = TRUE)

## Save lists of initial and final imports and outputs by country
write.csv(ExportsFinal, paste0("outputs/data_network/",runname, "ExportsFinal.csv"), row.names = FALSE)
write.csv(ExportsInitial, paste0("outputs/data_network/",runname, "ExportsInitial.csv"), row.names = FALSE)
write.csv(ImportsFinal, paste0("outputs/data_network/",runname, "ImportsFinal.csv"), row.names = FALSE)
write.csv(ImportsInitial, paste0("outputs/data_network/",runname, "ImportsInitial.csv"), row.names = FALSE)

# Save Exports as R data file
saveRDS(Eout, file = paste0("outputs/data_network/", runname,"ExportStatic.rds"))

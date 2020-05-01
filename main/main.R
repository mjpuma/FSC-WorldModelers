## Main script for the Food Shock Cascade (FSC) Model

# Create output directory if needed
if (dir.exists("outputs") == FALSE) {
  dir.create("outputs")
}

# Step 1: Input Arguments ----
# RStudio version: Specify arguments  ====
setwd("~/GitHub_mjpuma/FSC-WorldModelers/")

# Set year range to run model
years <- 1:4
# Specify model version to run: 0-> PTA; 1-> RTA
FSCversion = 0

# Command line version: Parse arguments ====
# args <- commandArgs(trailingOnly = TRUE)
# years <- c(as.numeric(args[1]))
# country <- args[2]
# production_decrease <- as.numeric(args[3])
# fractional_reserve_access <- as.numeric(args[4])
# output_file_name <- args[5]

# Step 2: Load FSC functions ----
source("main/FSC_component_funcs.R")
source("main/FSC_sim_funcs.R")
library(dplyr, warn.conflicts = FALSE)
library(tidyr)

# Step 3: Load ancillary data ----
# 1)  Commodity list for bilateral trade
commodities <- read.csv("ancillary/cropcommodity_tradelist.csv")
# 2) Load country list
country_list <- read.csv("ancillary/country_list195_2012to2016.csv")

# Step 4: Load production/trade/stocks data ----
load("inputs_processed/E0.RData") #Export Matrix ordered by FAOSTAT country code (increasing)
load("inputs_processed/P0.Rdata") #Production
load("inputs_processed/R0.RData") #Reserves (a.k.a. Stocks)

# Step 5: Load production *fractional declines* list by year by country ----
anomalies <-
  read.csv("inputs/Prod_DeclineFraction_DustBowl_195countries.csv")

# Step 6: Setup production and shocks; initialize output vectors ----
# Assign production vector to P0 ====
P0 <- Pkbyc
colnames(P0)[1] <- "iso3"

# Create 'Shocks' dataframe ====
Shocks <-
  merge(country_list,
        anomalies,
        by = 'iso3')
Shocks[is.na(Shocks)] <- 0

# Order shocks dataframe by FAOSTAT country code (in increasing order) ====
P <- P0
Shocks <- merge(Shocks, P, by = "iso3")
Shocks <- Shocks[order(Shocks$FAO),]
Prod <- as.numeric(unlist(P0$P0))

# Initialize  output vectors ====
Pout <-  array(0, c(nrow(country_list), length(years) + 1))
Rout <-  array(0, c(nrow(country_list), length(years) + 1))
Eout <-  array(0, c(nrow(country_list), nrow(country_list), length(years) + 1))

shortageout <- array(0, c(nrow(country_list), length(years)))
C_C0out <- array(0, c(nrow(country_list), length(years)))
dR_C0out <- array(0, c(nrow(country_list), length(years)))

## Add initial conditions to output arrays
Pout[, 1] <- Prod
Rout[, 1] <- R0
Eout[, , 1] <- E0

## Create 'InputFSC' dataframe adding initial reserves====
InputFSC <- data_frame(iso3 = names(R0), R0 = R0)

# Step 7: Time loop (annual timestep, updating InputFSC) ----
for (i in 1:length(years)) {
  ## Update progress in time loop
  cat('Timestep', i, 'of', length(years), '\n')
  
  # Separate NEGATIVE and POSITIVE shock anomalies ====
  #   Fractional gains and losses in production
  #   Note: Initial production, P0, is fixed but Shocks vary in time
  FracGain <- Shocks[i + 3]
  FracGain[FracGain > 0] <- 0
  FracGain <-
    -FracGain # adjust sign because fractional declines read in
  
  FracLoss <- Shocks[i + 3]
  FracLoss[FracLoss < 0] <- 0
  
  # Create vector for NEGATIVE shock anomalies ====
  # Adjust sign because fractional declines read in
  dP <- -FracLoss * Shocks$P0
  Shocks$dP <- dP
  
  # Set Reserves and add POSTIVE anomalies to reserves ====
  if (i == 1) {
    ## First timestep: add Shocks into InputFSC dataframe
    InputFSC <-
      merge(Shocks,
            InputFSC,
            by = 'iso3',
            all.x = TRUE,
            all.y = FALSE)
    InputFSC <- InputFSC[order(InputFSC$FAO), ]
    
    # Add positive anomalies to reserves ====
    InputFSC$Rcurrent <- InputFSC$R0  + (InputFSC$P0 * FracGain)
    Rcurrent <- as.numeric(unlist(InputFSC$Rcurrent))
    
  } else {
    # Update reserve levels: use ending levels from previous timestep and add production gains
    Rcurrent <- Rcurrent + results_FSC$dR  + InputFSC$P0 * FracGain
    Rcurrent <- as.numeric(unlist(Rcurrent))
    
  }
  
  # Update state variables for trade_dat ====
  if (i == 1) {
    # Assign production, reserves, and export matrix
    trade_dat <- list(P = Prod, R = Rcurrent, E = E0)
    # Number of countries
    trade_dat$nc <- length(trade_dat$P)
    # Change in reserves; set initially to zero
    trade_dat$dR <- rep(0, trade_dat$nc)
    # Compute consumption assuming that it is initially equal to supply
    trade_dat$C <- get_supply(trade_dat)
    C0_initial <- trade_dat$C
    # Initial shortage is 0
    trade_dat$shortage <-
      rep(0, trade_dat$nc)
  } else {
    # Clear trade_dat dataframe
    rm(trade_dat)
    # Assign production, reserves *from previous timestep*, and export matrix
    #   i.e. update only reserves; leave consumption & trade at initial levels
    trade_dat <- list(P = Prod, R = Rcurrent, E = E0)
    # Number of countries
    trade_dat$nc <- length(trade_dat$P)
    # Change in reserves
    trade_dat$dR <- rep(0, trade_dat$nc)
    # Initial shortage is 0
    trade_dat$shortage <- rep(0, trade_dat$nc)
    # Compute consumption assuming that it is initially equal to supply
    trade_dat$C <- get_supply(trade_dat)
    # Clear results from previous timestep
    rm(results_FSC)
  }
  
  # Call main simulation functions
  dP <- as.numeric(unlist(Shocks$dP))
  if (FSCversion == 0) {
    results_FSC <-
      sim_cascade_PTA(trade_dat, dP)  # Run Proportional Trade Allocation (PTA) Model
  } else if (FSCversion == 1) {
    results_FSC <-
      sim_cascade_RTA(trade_dat, dP)  # Run Reserves-based Trade Allocation (RTA) Model
  }
  
  # Calculate and store outputs of interest from results ====
  #   Output as 1D arrays
  Pout[, i + 1]    <- results_FSC$P
  Rout[, i + 1]    <- Rcurrent + results_FSC$dR
  shortageout[, i] <- results_FSC$shortage
  #     consumption relative to initial consumption
  C_C0out[, i] <- results_FSC$C / C0_initial
  #     change in reserves relative to initial consumption
  dR_C0out[, i] <- results_FSC$dR / C0_initial
  #   Output as 2D arrays
  Eout[, , i + 1]  <- results_FSC$E
  
  # Clear unneeded variables
  Shocks <- select(Shocks,-dP)
  rm(FracGain)
  rm(FracLoss)
  
}

# Step 8: Collect, reformat and save output data ----

## Add row aand column names to output files
column_names = c('0', '1', '2', '3', '4')
column_names2 = c('1', '2', '3', '4')

# Production
colnames(Pout)  <- column_names
rownames(Pout)  <- InputFSC$iso3
Pout_df <- data.frame(Pout)
Pout_df <- tibble::rownames_to_column(Pout_df, "iso3")
Pout_df <- merge(InputFSC[, c("iso3", "Country")], Pout_df, by = "iso3")
# combine the year columns into a single column with separate rows for each year; assign to new vector
Pout_df <- gather(Pout_df, Year, Value, -iso3, -Country)
# remove preceeding X character for Year column aand convert to numeric
Pout_df$Year <- as.numeric(gsub("[a-zA-Z ]", "", Pout_df$Year))

# Reserves
colnames(Rout)  <- column_names
rownames(Rout)  <- InputFSC$iso3
Rout_df <- data.frame(Rout)
Rout_df <- tibble::rownames_to_column(Rout_df, "iso3")
Rout_df <- merge(InputFSC[, c("iso3", "Country")], Rout_df, by = "iso3")
# combine the year columns into a single column with separate rows for each year; assign to new vector
Rout_df <- gather(Rout_df, Year, Value, -iso3, -Country)
# remove preceeding X character for Year column aand convert to numeric
Rout_df$Year <- as.numeric(gsub("[a-zA-Z ]", "", Rout_df$Year))

# Shortage
colnames(shortageout)  <- column_names2
rownames(shortageout)  <- InputFSC$iso3
shortageout_df <- data.frame(shortageout)
shortageout_df <- tibble::rownames_to_column(shortageout_df, "iso3")
shortageout_df <-
  merge(InputFSC[, c("iso3", "Country")], shortageout_df, by = "iso3")
# combine the year columns into a single column with separate rows for each year; assign to new vector
shortageout_df <- gather(shortageout_df, Year, Value, -iso3, -Country)
# remove preceeding X character for Year column aand convert to numeric
shortageout_df$Year <-
  as.numeric(gsub("[a-zA-Z ]", "", shortageout_df$Year))

# Consumption to C0
colnames(C_C0out)  <- column_names2
rownames(C_C0out)  <- InputFSC$iso3
C_C0out_df <- data.frame(C_C0out)
C_C0out_df <- tibble::rownames_to_column(C_C0out_df, "iso3")
C_C0out_df <-
  merge(InputFSC[, c("iso3", "Country")], C_C0out_df, by = "iso3")
# combine the year columns into a single column with separate rows for each year; assign to new vector
C_C0out_df <- gather(C_C0out_df, Year, Value, -iso3, -Country)
# remove preceeding X character for Year column aand convert to numeric
C_C0out_df$Year <- as.numeric(gsub("[a-zA-Z ]", "", C_C0out_df$Year))

# Change in Reserves to C0
colnames(dR_C0out)  <- column_names2
rownames(dR_C0out)  <- InputFSC$iso3
dR_C0out_df <- data.frame(dR_C0out)
dR_C0out_df <- tibble::rownames_to_column(dR_C0out_df, "iso3")
dR_C0out_df <-
  merge(InputFSC[, c("iso3", "Country")], dR_C0out_df, by = "iso3")
# combine the year columns into a single column with separate rows for each year; assign to new vector
dR_C0out_df <- gather(dR_C0out_df, Year, Value, -iso3, -Country)
# remove preceeding X character for Year column aand convert to numeric
dR_C0out_df$Year <-
  as.numeric(gsub("[a-zA-Z ]", "", dR_C0out_df$Year))

# Export matrix
colnames(Eout)  <- InputFSC$iso3
rownames(Eout)  <- InputFSC$iso3

# Save Exports as R data file
saveRDS(Eout, file = "outputs/ExportSeries.rds")

## Save as CSV
write.csv(Pout_df, "outputs/ProductionSeries.csv", row.names = FALSE)
write.csv(Rout_df, "outputs/ReserveSeries.csv", row.names = FALSE)
write.csv(shortageout_df, "outputs/ShortageSeries.csv", row.names = FALSE)
write.csv(C_C0out_df, "outputs/ConsumptiontoC0Series.csv", row.names = FALSE)
write.csv(dR_C0out_df,
          "outputs/ReserveChangetoC0Series.csv",
          row.names = FALSE)
write.csv(Eout, "outputs/ExportSeries.csv", row.names = TRUE)

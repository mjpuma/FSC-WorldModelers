## Main script for the Food Shock Cascade (FSC) Model

setwd("~/GitHub_mjpuma/FSC-WorldModelers/")            ###Remove before commit

# Create output directory if needed
if (dir.exists("outputs") == FALSE) {
  dir.create("outputs")
}

# Step 1: Input Arguments ----
# Command line version: Parse arguments ====
# args <- commandArgs(trailingOnly = TRUE)
# years <- c(as.numeric(args[1]))
# country <- args[2]
# production_decrease <- as.numeric(args[3])
# fractional_reserve_access <- as.numeric(args[4])
# output_file_name <- args[5]

# rstudio version: Specify arguments  ====
years <- 1:4  # Set year range to run model

# Step 2: Load FSC functions ----
source("main/FSC_component_funcs.R")
source("main/FSC_sim_funcs.R")
library(dplyr, warn.conflicts = FALSE)

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

# Step 6: Setup production and shocks; initialize output  vectors ----
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
Shocks <- Shocks[order(Shocks$FAO), ]
Prod <- as.numeric(unlist(P0$P0))

# Initialize  output vectors ====
## PTA version
P_PTA <-  array(0, c(nrow(country_list), length(years) + 1))
R_PTA <-  array(0, c(nrow(country_list), length(years) + 1))
E_PTA <-
  array(0, c(nrow(country_list), nrow(country_list), length(years) + 1))

shortage_PTA <- array(0, c(nrow(country_list), length(years)))
C_C0_PTA <- array(0, c(nrow(country_list), length(years)))
dR_C0_PTA <- array(0, c(nrow(country_list), length(years)))

## RTA version
P_RTA <-  array(0, c(nrow(country_list), length(years) + 1))
R_RTA <-  array(0, c(nrow(country_list), length(years) + 1))
E_RTA <-
  array(0, c(nrow(country_list), nrow(country_list), length(years) + 1))

shortage_RTA <- array(0, c(nrow(country_list), length(years)))
C_C0_RTA <- array(0, c(nrow(country_list), length(years)))
dR_C0_RTA <- array(0, c(nrow(country_list), length(years)))

## Add initial conditions to output arrays
## PTA version
P_PTA[, 1] <- Prod
R_PTA[, 1] <- R0
E_PTA[, , 1] <- E0

## RTA version
P_RTA[, 1] <- Prod
R_RTA[, 1] <- R0
E_RTA[, , 1] <- E0

## Create 'Reserves' dataframe ====
Reserves <- data_frame(iso3 = names(R0), R0 = R0)

# Step 7: Time loop (annual timestep, updating Reserves) ----
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
  Shocks$dP <-
    (-FracLoss) * (Shocks$P0)  # adjust sign because fractional declines read in
  
  dP <- as.numeric(unlist(Shocks$dP))
  
  # Set Reserves and add POSTIVE anomalies to reserves ====
  if (i == 1) {
    ## First timestep, merge Shocks into Reserves dataframe
    Reserves <-
      merge(Shocks,
            Reserves,
            by = 'iso3',
            all.x = TRUE,
            all.y = FALSE)
    Reserves <- Reserves[order(Reserves$FAO), ]
    
    # Add positive anomalies to reserves ====
    Reserves$R1 <- Reserves$R0  + (Reserves$P0 * FracGain)
    R1_PTA <- as.numeric(unlist(Reserves$R1))
    R1_RTA <- as.numeric(unlist(Reserves$R1))
    } else {
    # Update reserve levels: use ending levels from previous timestep and add production gains
    R1_PTA <- results_FSC_PTA$R  + Reserves$P0 * FracGain
    R1_PTA <- as.numeric(unlist(R1_PTA))
    
    #R1_RTA <- results_FSC_RTA$R  + Reserves$P0 * FracGain
    #R1_RTA <- as.numeric(unlist(R1_RTA))
    }
  
  # Update state variables for trade_dat and run PTA version ====
  if (i == 1) {
    # Assign production, reserves, and export matrix
    trade_dat_PTA <- list(P = Prod, R = R1_PTA, E = E0)
    # Number of countries
    trade_dat_PTA$nc <- length(trade_dat_PTA$P)
    # Change in reserves
    trade_dat_PTA$dR <- rep(0, trade_dat_PTA$nc)
    # Initial shortage is 0
    trade_dat_PTA$shortage <- rep(0, trade_dat_PTA$nc)
    # Compute consumption assuming that it is initially equal to supply
    trade_dat_PTA$C <- get_supply(trade_dat_PTA)
    } else {
    # Clear trade_dat dataframe
    rm(trade_dat_PTA)
    # Assign production, reserves *from previous timestep*, and export matrix
    #   i.e. update only reserves; leave consumption & trade at initial levels
    trade_dat_PTA <- list(P = Prod, R = R1_PTA, E = E0)
    # Number of countries
    trade_dat_PTA$nc <- length(trade_dat_PTA$P)
    # Change in reserves
    trade_dat_PTA$dR <- rep(0, trade_dat_PTA$nc)
    # Initial shortage is 0
    trade_dat_PTA$shortage <- rep(0, trade_dat_PTA$nc)
    # Compute consumption assuming that it is initially equal to supply
    trade_dat_PTA$C <- get_supply(trade_dat_PTA)
    # Clear results from previous timestep
    rm(results_FSC_PTA)
    }
  
  # Call main simulation functions
  results_FSC_PTA <- sim_cascade_PTA(trade_dat_PTA, dP)  # Run Proportional Trade Allocation (PTA) Model
  
  # Update state variables for trade_dat and run RTA version ====
  if (i == 1) {
    # Assign production, reserves, and export matrix
    trade_dat_RTA <- list(P = Prod, R = R1_RTA, E = E0)
    # Number of countries
    trade_dat_RTA$nc <-length(trade_dat_RTA$P)
    # Change in reserves
    trade_dat_RTA$dR <-rep(0, trade_dat_RTA$nc)
    # Compute consumption assuming that it is initially equal to supply
    trade_dat_RTA$C <- get_supply(trade_dat_RTA)
    # Initial shortage is 0
    trade_dat_RTA$shortage <-
      rep(0, trade_dat_RTA$nc)
    } else {
    # Clear trade_dat dataframe
    rm(trade_dat_RTA)
    # Assign production, reserves *from previous timestep*, and export matrix
    #   i.e. update only reserves; leave consumption & trade at initial levels
    trade_dat_RTA <- list(P = Prod, R = R1_RTA, E = E0)
    # Number of countries
    trade_dat_RTA$nc <- length(trade_dat_RTA$P)
    # Change in reserves
    trade_dat_RTA$dR <- rep(0, trade_dat_RTA$nc)
    # Initial shortage is 0
    trade_dat_RTA$shortage <- rep(0, trade_dat_RTA$nc)
    # Compute consumption assuming that it is initially equal to supply
    trade_dat_RTA$C <- get_supply(trade_dat_RTA)
    # Clear results from previous timestep
    rm(results_FSC_RTA)
    }
  
  # Call main simulation functions
  results_FSC_RTA <- sim_cascade_RTA(trade_dat_RTA, dP)  # Run Reserves-based Trade Allocation (RTA) Model
  
  # Calculate and store outputs of interest from results ====
  ## PTA version
  P_PTA[, i + 1]    <- results_FSC_PTA$P
  R_PTA[, i + 1]    <- results_FSC_PTA$R
  E_PTA[, , i + 1]  <- results_FSC_PTA$E
  shortage_PTA[, i] <- results_FSC_PTA$shortage
  C_C0_PTA[, i] <-
    results_FSC_PTA$C / trade_dat_PTA$C # consumption relative to initial consumption
  dR_C0_PTA[, i] <-
    results_FSC_PTA$dR / trade_dat_PTA$C # change in reserves relative to initial consumption
  
  ## RTA version
  P_RTA[, i + 1]    <- results_FSC_RTA$P
  R_RTA[, i + 1]    <- results_FSC_RTA$R
  E_RTA[, , i + 1]  <- results_FSC_RTA$E
  shortage_RTA[, i] <- results_FSC_RTA$shortage
  C_C0_RTA[, i] <-
    results_FSC_RTA$C / trade_dat_RTA$C # consumption relative to initial consumption
  dR_C0_RTA[, i] <-
    results_FSC_RTA$dR / trade_dat_RTA$C # change in reserves relative to initial consumption
}

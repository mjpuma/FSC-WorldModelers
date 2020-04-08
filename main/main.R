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

# Specify model version to run: 0-> PTA; 1-> RTA
FSCversion = 0

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
P <-  array(0, c(nrow(country_list), length(years) + 1))
R <-  array(0, c(nrow(country_list), length(years) + 1))
E <-
  array(0, c(nrow(country_list), nrow(country_list), length(years) + 1))

shortage <- array(0, c(nrow(country_list), length(years)))
C_C0 <- array(0, c(nrow(country_list), length(years)))
dR_C0 <- array(0, c(nrow(country_list), length(years)))

## Add initial conditions to output arrays
P[, 1] <- Prod
R[, 1] <- R0
E[, , 1] <- E0

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
  
  # Set Reserves and add POSTIVE anomalies to reserves ====
  if (i == 1) {
    ## First timestep, merge Shocks into Reserves dataframe
    Reserves <-
      merge(Shocks,
            Reserves,
            by = 'iso3',
            all.x = TRUE,
            all.y = FALSE)
    Reserves <- Reserves[order(Reserves$FAO),]
    
    # Add positive anomalies to reserves ====
    Reserves$R1 <- Reserves$R0  + (Reserves$P0 * FracGain)
    R1 <- as.numeric(unlist(Reserves$R1))
  } else {
    # Update reserve levels: use ending levels from previous timestep and add production gains
    R1 <- results_FSC$R  + Reserves$P0 * FracGain
    R1 <- as.numeric(unlist(R1))
  }
  
  
  # Update state variables for trade_dat ====
  if (i == 1) {
    # Assign production, reserves, and export matrix
    trade_dat <- list(P = Prod, R = R1, E = E0)
    # Number of countries
    trade_dat$nc <-length(trade_dat$P)
    # Change in reserves
    trade_dat$dR <-rep(0, trade_dat$nc)
    # Compute consumption assuming that it is initially equal to supply
    trade_dat$C <- get_supply(trade_dat)
    # Initial shortage is 0
    trade_dat$shortage <-
      rep(0, trade_dat$nc)
    } else {
    # Clear trade_dat dataframe
    rm(trade_dat)
    # Assign production, reserves *from previous timestep*, and export matrix
    #   i.e. update only reserves; leave consumption & trade at initial levels
    trade_dat <- list(P = Prod, R = R1, E = E0)
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
  P[, i + 1]    <- results_FSC$P
  R[, i + 1]    <- results_FSC$R
  E[, , i + 1]  <- results_FSC$E
  shortage[, i] <- results_FSC$shortage
  C_C0[, i] <-
    results_FSC$C / trade_dat$C # consumption relative to initial consumption
  dR_C0[, i] <-
    results_FSC$dR / trade_dat$C # change in reserves relative to initial consumption
}

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
years <- 2012:2016  # Set year range for production, trade, and reserves data

# Step 2: Load FSC functions ----
source("main/FSC_component_funcs.R")
source("main/FSC_sim_funcs.R")
library(dplyr, warn.conflicts = FALSE)

# Step 3: Load ancillary data ----
# 1)  Commodity list for bilateral trade
commodities<-read.csv("ancillary/cropcommodity_tradelist.csv")
# 2) Load country list
country_list <- read.csv("ancillary/country_list195_2012to2016.csv")

# Step 4: Load production/trade/stocks data ----
load("inputs_processed/E0.RData") #Export Matrix ordered by FAOSTAT country code (increasing)
load("inputs_processed/P0.Rdata") #Production
load("inputs_processed/R0.RData") #Reserves (a.k.a. Stocks)

# Step 5: Load production aanomalies list by year by country ----
anomalies<-read.csv("inputs/Prod_DeclineFraction_5Years_195countires.csv")

# Step 6: Setup production and shocks; initialize output  vectors ----

# Assign production vector to P0 ====
P0<-Pkbyc 
colnames(P0)[1] <- "iso3"

# Create 'Shocks' dataframe ====
Shocks <- 
  merge(
    country_list,
    anomalies,
    by = 'iso3'
  )
Shocks[is.na(Shocks)] <- 0

# Order shocks dataframe by FAOSTAT country code (in increasing order) ====
P <- P0
Shocks <- merge(Shocks, P, by = "iso3")
Shocks <- Shocks[order(Shocks$FAO), ]
Prod <-as.numeric(unlist(P0$P0))

# Initialize  output vectors ====
shortage_PTA <- array(0,c(nrow(country_list),length(years)))
C_C0_PTA <- array(0,c(nrow(country_list),length(years)))
dR_C0_PTA <- array(0,c(nrow(country_list),length(years)))
dE_PTA <- array(0,c(nrow(country_list),nrow(country_list),length(years)))

shortage_RTA <- array(0,c(nrow(country_list),length(years)))
C_C0_RTA <- array(0,c(nrow(country_list),length(years)))
dR_C0_RTA <- array(0,c(nrow(country_list),length(years)))
dE_RTA <- array(0,c(nrow(country_list),nrow(country_list),length(years)))

# Step 7: Time loop (annual timestep, updating Reserves) ----
for (i in 1:length(years)) {
  
  ## Update progress in time loop
  cat('Timestep', i, 'of', length(years),'\n')
  
  ## Separate fractional gains and losses in production ====
  FracGain<- Shocks[i+3]
  FracGain[FracGain > 0] <- 0
  
  FracLoss<- Shocks[i+3]
  FracLoss[FracLoss < 0] <- 0
  Shocks$dP <- (-FracLoss) * (Shocks$P0)
  
  ## Create 'Reserves' dataframe ====
  Reserves <- data_frame(iso3 = names(R0), R0 = R0)
  Reserves <-
    merge(Shocks,
          Reserves,
          by = 'iso3',
          all.x = TRUE,
          all.y = FALSE)
  Reserves <- Reserves[order(Reserves$FAO), ]
  
  # Add positive anomalies to reserves ====
  #       R0=R1 where no production increases occurred
  Reserves$R1 <- (Reserves$P0 * FracGain) + Reserves$R0
  identical(Reserves$R0[FracGain == 0], Reserves$R1[FracGain ==0]) 
  R1 <-as.numeric(unlist(Reserves$R1))
  #R1 <- data_frame(iso3 = names(R0), Reserves$R1)
  ##R1 <- (Reserves$R1)
  ##names(R1) <- Reserves$iso3
  
  # State variables to trade_dat ====
  trade_dat<-list(P = Prod, R = R1, E = E0)
  trade_dat$nc <- length(trade_dat$P)        # number of countries
  trade_dat$dR <- rep(0, trade_dat$nc)       # change in reserves

  # Compute consumption assuming that it is initially equal to supply ====
  trade_dat$C <- get_supply(trade_dat)
  trade_dat$shortage <- rep(0, trade_dat$nc) # initial shortage is 0
  
  # Set initial shock ====
  dP <-as.numeric(unlist(Shocks$dP))
  #names(dP) <- names(trade_dat$P)
  ##dP <- Shocks$dP 
  ##names(dP) <- Shocks$iso3
  
  # Call main simulation functions ====
  results_FSC_PTA <- sim_cascade_PTA(trade_dat, dP)  # Run the FSC Reserves-based Trade Allocation Model (FSC-PTA)
  results_FSC_RTA <- sim_cascade_RTA(trade_dat, dP)  # Run the FSC Reserves-based Trade Allocation Model (FSC-RTA)
  
  # Calculate outputs of interest from results ====
  shortage_PTA[,i] <- results_FSC_PTA$shortage
  C_C0_PTA[,i] <- results_FSC_PTA$C / trade_dat$C # consumption relative to initial consumption
  dR_C0_PTA[,i] <- results_FSC_PTA$dR / trade_dat$C # change in reserves relative to initial consumption
  dE_PTA[, ,i] <- results_FSC_PTA$E - trade_dat$E # change in trade

  shortage_RTA[,i] <- results_FSC_PTA$shortage  
  C_C0_RTA[,i] <- results_FSC_RTA$C / trade_dat$C # consumption relative to initial consumption
  dR_C0_RTA[,i] <- results_FSC_RTA$dR / trade_dat$C # change in reserves relative to initial consumption
  dE_RTA[, ,i] <- results_FSC_RTA$E - trade_dat$E # change in trade
  
  # Update reserve levels for next timestep ====
  #R1<-((results_FSC_RTA$R)+(results_FSC_RTA$P*results_FSC_RTA$Inc))
  
}

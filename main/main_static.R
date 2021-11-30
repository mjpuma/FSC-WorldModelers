## Main script for the Static Food Shock Cascade (FSC) Model
# Computes impaired supply for scenraios of production anomalies and trade intervensions
# Output includes network statistics based on on export matrix ----
#    https://www.r-bloggers.com/network-centrality-in-r-an-introduction/
#    https://kateto.net/networks-r-igraph
#    https://kateto.net/netscix2016.html


# Load required packages
library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(igraph, warn.conflicts = FALSE)
library(ggraph, warn.conflicts = FALSE)
library(tidyverse, warn.conflicts = FALSE)
library(netrankr, warn.conflicts = FALSE)
library("stringr")

# Step 0: Setup ----
# Default 1 year run for static model
years <- 1 #Don't Change
baseyear <- 2020
# Create column names for output files
column_names = c('0', '1')
column_names2 = c('1')
column_names1 = c('0')

# Create output directory if needed
if (dir.exists("outputs") == FALSE) {
  dir.create("outputs")
}

# Step 1: Specify scenario ----
#  Command line: Rscript main/main.R "/Users/puma/GitHub_mjpuma/FSC-WorldModelers/" "wheat" 0.2 0.2
# Parse arguments ====
#args <- commandArgs(trailingOnly = TRUE)
#working_directory <- c(args[1])                 # Directory
#crop <- c(args[2])                              # Crop commodity group
#anomaly_factor <- c(as.numeric(args[3]))        # Anomaly factor
#restriction_intensity <- c(as.numeric(args[4])) # Restriction intensity

# #  RStudio or similar integrated development environment (IDE)
#  Specify arguments  ====
working_directory <- "/Users/puma/GitHub_mjpuma/FSC-WorldModelers/"  # Directory
crop <- "rice"                              # Crop commodity group
anomaly_factor <- 0.2                          # Anomaly factor
restriction_intensity <- 0.2                    # Restriction intensity

anomaly_factor_string <- gsub("\\.","p",format(round(anomaly_factor, 2), nsmall = 2))
restriction_intensity_string <- gsub("\\.","p",format(round(restriction_intensity, 2), nsmall = 2))

# Step 2: Scenario library (read in files) ----
# Production *fractional declines* list by year by country ====

name_crop <-str_to_sentence(crop)
nameinput <- paste0(name_crop,'_Avg20152017')
runname <- paste0(crop,'_pa-',anomaly_factor_string,'_er-',restriction_intensity_string)
nameinput <- paste0(name_crop,'_Avg20152017')
shock_scenario <- read.csv(paste0(working_directory,'inputs/2021_C2P2_scenarios/Scenario_',crop,'_total_production_decline.csv'))


# Step 3: Load ancillary data ----
# i) Commodity list for bilateral trade
commodities <- read.csv(paste0(working_directory,"ancillary/", nameinput, "cropcommodity_tradelist.csv"))
# ii) Load country list
country_list <- read.csv(paste0(working_directory,"ancillary/country_list195_2012to2016.csv"))

# Step 4: Load production/trade/stocks data ----
load(paste0(working_directory,"inputs_processed/", nameinput, "E0.RData")) #Export Matrix ordered by FAOSTAT country code (increasing)
load(paste0(working_directory,"inputs_processed/", nameinput, "P0.Rdata")) #Production
load(paste0(working_directory,"inputs_processed/", nameinput, "R0.RData")) #Reserves (a.k.a. Stocks)


# Step 5: Setup production and shocks; initialize output vectors ----
# Assign production vector to P0 ====
P0 <- Pkbyc
# Create 'Shocks' dataframe ====
Shocks <- plyr::join(country_list,shock_scenario, by = 'iso3')

Shocks[is.na(Shocks)] <- 0

Shocks$year_1 <- anomaly_factor * Shocks$year_1

# Order shocks dataframe by FAOSTAT country code (in increasing order) ====

P <- P0
Prod <- as.numeric(unlist(P0$P0))
Shocks <- plyr::join(Shocks, P, by = 'iso3')


# Initialize  output vectors ====
Pout <-  array(0, c(nrow(country_list), length(years) + 1))
Rout <-  array(0, c(nrow(country_list), length(years) + 1))
Eout <-  array(0, c(nrow(country_list), nrow(country_list), length(years) + 1))

ImpairedSupply <- array(0, c(nrow(country_list), length(years)))
ImpairedSupplyToReserves <- array(0, c(nrow(country_list), length(years)))


P_initial_out <-  array(0, c(nrow(country_list), length(years)))
P_final_out <-  array(0, c(nrow(country_list), length(years)))
R_initial_out <-  array(0, c(nrow(country_list), length(years)))
R_final_out <-  array(0, c(nrow(country_list), length(years)))

E_initial_out <-  array(0, c(nrow(country_list), length(years)))
E_final_out <-  array(0, c(nrow(country_list), length(years)))
I_initial_out <-  array(0, c(nrow(country_list), length(years)))
I_final_out <-  array(0, c(nrow(country_list), length(years)))

## Add initial conditions to output arrays
Eout[, , 1] <- E0
Pout[, 1] <- Prod
Rout[, 1] <- R0

P_initial_out[, 1] <- Prod
R_initial_out[, 1] <- R0


## Create 'InputFSC' dataframe adding initial reserves====
InputFSC <- tibble(iso3 = names(R0), R0 = R0)


# Step 6:
# Separate NEGATIVE and POSITIVE shock anomalies ====
#   Fractional gains and losses in production
#   Note: Initial production, P0, is fixed but Shocks vary in time
FracGain <- Shocks$year_1
FracGain <- -FracGain      # adjust sign (fractional *declines* read in)

FracLoss <- Shocks[4]
FracLoss <- -FracLoss      # adjust sign (fractional *declines* read in)

# Create vector for NEGATIVE shock anomalies ====
dP <-  FracLoss * Shocks$P0 
Shocks$dP <- dP

# Set Combine Shocks and Set Reserves ====
InputFSC <- plyr::join(Shocks,InputFSC, by = 'iso3')
InputFSC$Rcurrent <- InputFSC$R0  

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

# BASELINE SUPPLY
Supply_baseline <- trade_dat$C

# Domestic Supply after production declines
trade_dat$C1 <- trade_dat$P + Shocks$dP + colSums(trade_dat$E) - rowSums(trade_dat$E)

ImportsInitial<-colSums(trade_dat$E)
ExportsInitial<-rowSums(trade_dat$E)

I_initial_out[,1]<-colSums(trade_dat$E)
E_initial_out[,1]<-rowSums(trade_dat$E)

# Step 7:
# Specify and impose export restrictions     
if (crop == 'wheat') {
  # export_scenario: Scenario_Wheat_ExportRestrictionFraction_Year2008_with_RUS_25%_195countries.csv
     i_restrict = which(country_list[,2] =='RUS')
     reduce_factor <- restriction_intensity * 0.5
     #   Add *restricted* exports to reserves
     Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
     #  Impose export restrictions by reducing exports
     E0[i_restrict,]<- (1. - reduce_factor)*E0[i_restrict,]

     i_restrict = which(country_list[,2] =='ARG')
     reduce_factor <- restriction_intensity * 12./12.
     #   Add *restricted* exports to reserves
     Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
     #  Impose export restrictions by reducing exports
     E0[i_restrict,]<- (1. - reduce_factor)*E0[i_restrict,]

     i_restrict = which(country_list[,2] =='BOL')
     reduce_factor <- restriction_intensity * 1.5/12.
     #   Add *restricted* exports to reserves
     Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
     #  Impose export restrictions by reducing exports
     E0[i_restrict,]<- (1. - reduce_factor)*E0[i_restrict,]

     i_restrict = which(country_list[,2] =='ETH')
     reduce_factor <- restriction_intensity * 12./12.
     #   Add *restricted* exports to reserves
     Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
     #  Impose export restrictions by reducing exports
     E0[i_restrict,]<- (1. - reduce_factor)*E0[i_restrict,]
     
     
     i_restrict = which(country_list[,2] =='IND')
     reduce_factor <- restriction_intensity * 12./12.
     #   Add *restricted* exports to reserves
     Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
     #  Impose export restrictions by reducing exports
     E0[i_restrict,]<- (1. - reduce_factor)*E0[i_restrict,]
     

     i_restrict = which(country_list[,2] =='KAZ')
     reduce_factor <- restriction_intensity * 2./12.
     #   Add *restricted* exports to reserves
     Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
     #  Impose export restrictions by reducing exports
     E0[i_restrict,]<- (1. - reduce_factor)*E0[i_restrict,]
     
     i_restrict = which(country_list[,2] =='NPL')
     reduce_factor <- restriction_intensity * 4./12.
     #   Add *restricted* exports to reserves
     Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
     #  Impose export restrictions by reducing exports
     E0[i_restrict,]<- (1. - reduce_factor)*E0[i_restrict,]
     
     i_restrict = which(country_list[,2] =='PAK')
     reduce_factor <- restriction_intensity * 12./12.
     #   Add *restricted* exports to reserves
     Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
     #  Impose export restrictions by reducing exports
     E0[i_restrict,]<- (1. - reduce_factor)*E0[i_restrict,]
     
     i_restrict = which(country_list[,2] =='SRB')
     reduce_factor <- restriction_intensity * 0./12.
     #   Add *restricted* exports to reserves
     Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
     #  Impose export restrictions by reducing exports
     E0[i_restrict,]<- (1. - reduce_factor)*E0[i_restrict,]

     i_restrict = which(country_list[,2] =='SYR')
     reduce_factor <- restriction_intensity * 11./12.
     #   Add *restricted* exports to reserves
     Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
     #  Impose export restrictions by reducing exports
     E0[i_restrict,]<- (1. - reduce_factor)*E0[i_restrict,]
     
     i_restrict = which(country_list[,2] =='UKR')
     reduce_factor <- restriction_intensity * 0./12.
     #   Add *restricted* exports to reserves
     Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
     #  Impose export restrictions by reducing exports
     E0[i_restrict,]<- (1. - reduce_factor)*E0[i_restrict,]
     
     i_restrict = which(country_list[,2] =='GIN')
     reduce_factor <- restriction_intensity * 12./12.
     #   Add *restricted* exports to reserves
     Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
     #  Impose export restrictions by reducing exports
     E0[i_restrict,]<- (1. - reduce_factor)*E0[i_restrict,] 
     
} else if (crop == 'corn') {
  # export_scenario: Scenario_Maize_ExportRestrictionFraction_Year2008_with_ARG_25%_195countries.csv
    i_restrict = which(country_list[,2] =='ARG')
    reduce_factor <- restriction_intensity * 0.5
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- (1. - reduce_factor)*E0[i_restrict,]

    # Export_scenario: Scenario_Maize_ExportRestrictionFraction_Year2008_195countries.csv
    
    i_restrict = which(country_list[,2] =='CHN')
    reduce_factor <- restriction_intensity * 9./12.
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- (1. - reduce_factor)*E0[i_restrict,]    
        
    i_restrict = which(country_list[,2] =='ETH')
    reduce_factor <- restriction_intensity * 8./12.
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- (1. - reduce_factor)*E0[i_restrict,]
    
    i_restrict = which(country_list[,2] =='GIN')
    reduce_factor <- restriction_intensity * 6./12.
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- (1. - reduce_factor)*E0[i_restrict,]   

    i_restrict = which(country_list[,2] =='IND')
    reduce_factor <- restriction_intensity * 3./12.
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- (1. - reduce_factor)*E0[i_restrict,]  
    
    i_restrict = which(country_list[,2] =='KEN')
    reduce_factor <- restriction_intensity * 0./12.
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- (1. - reduce_factor)*E0[i_restrict,]
    
    i_restrict = which(country_list[,2] =='NPL')
    reduce_factor <- restriction_intensity * 5./12.
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- (1. - reduce_factor)*E0[i_restrict,]

    i_restrict = which(country_list[,2] =='SRB')
    reduce_factor <- restriction_intensity * 10.5/12.
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- (1. - reduce_factor)*E0[i_restrict,]
    
     i_restrict = which(country_list[,2] =='UKR')
    reduce_factor <- restriction_intensity * 6./12.
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- (1. - reduce_factor)*E0[i_restrict,]   
    
     i_restrict = which(country_list[,2] =='ZMB')
    reduce_factor <- restriction_intensity * 7./12.
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- (1. - reduce_factor)*E0[i_restrict,]   

} else if (crop == 'rice') {
  # export_scenario: Scenario_Rice_ExportRestrictionFraction_Year2008_195countries.csv
    
    i_restrict = which(country_list[,2] =='ARG')
    reduce_factor <- restriction_intensity * 9./12.
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- (1. - reduce_factor)*E0[i_restrict,]
    
    
    i_restrict = which(country_list[,2] =='BGD')
    reduce_factor <- restriction_intensity * 2./12.
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- (1. - reduce_factor)*E0[i_restrict,]
    
    
        i_restrict = which(country_list[,2] =='BRA')
    reduce_factor <- restriction_intensity * 8./12.
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- (1. - reduce_factor)*E0[i_restrict,]

    i_restrict = which(country_list[,2] =='KHM')
    reduce_factor <- restriction_intensity * 10./12.
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- (1. - reduce_factor)*E0[i_restrict,]
    
    i_restrict = which(country_list[,2] =='CHN')
    reduce_factor <- restriction_intensity * 12./12.
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- (1. - reduce_factor)*E0[i_restrict,]
    
    i_restrict = which(country_list[,2] =='ECU')
    reduce_factor <- restriction_intensity * 12./12.
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- (1. - reduce_factor)*E0[i_restrict,]
    
    i_restrict = which(country_list[,2] =='EGY')
    reduce_factor <- restriction_intensity * 6./12.
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- (1. - reduce_factor)*E0[i_restrict,]

    i_restrict = which(country_list[,2] =='ETH')
    reduce_factor <- restriction_intensity * 12./12.
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- (1. - reduce_factor)*E0[i_restrict,]
    
        i_restrict = which(country_list[,2] =='IND')
    reduce_factor <- restriction_intensity * 10./12.
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- (1. - reduce_factor)*E0[i_restrict,]
    
    i_restrict = which(country_list[,2] =='IDN')
    reduce_factor <- restriction_intensity * 12./12.
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- (1. - reduce_factor)*E0[i_restrict,]

    
    i_restrict = which(country_list[,2] =='MDG')
    reduce_factor <- restriction_intensity * 7.5/12.
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- (1. - reduce_factor)*E0[i_restrict,]

    
    i_restrict = which(country_list[,2] =='MMR')
    reduce_factor <- restriction_intensity * 6./12.
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- (1. - reduce_factor)*E0[i_restrict,]
    
    
    i_restrict = which(country_list[,2] =='NPL')
    reduce_factor <- restriction_intensity * 6./12.
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- (1. - reduce_factor)*E0[i_restrict,]
    
    i_restrict = which(country_list[,2] =='PAK')
    reduce_factor <- restriction_intensity * 6./12.
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- (1. - reduce_factor)*E0[i_restrict,]
    

    i_restrict = which(country_list[,2] =='TZA')
    reduce_factor <- restriction_intensity * 8./12.
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- (1. - reduce_factor)*E0[i_restrict,]
    
    i_restrict = which(country_list[,2] =='THA')
    reduce_factor <- restriction_intensity * 8./12.
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- (1. - reduce_factor)*E0[i_restrict,]
    
    i_restrict = which(country_list[,2] =='VNM')
    reduce_factor <- restriction_intensity * 12./12.
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- (1. - reduce_factor)*E0[i_restrict,]


    i_restrict = which(country_list[,2] =='GIN')
    reduce_factor <- restriction_intensity * 9./12.
    #   Add *restricted* exports to reserves
    Rcurrent[i_restrict] = Rcurrent[i_restrict] + reduce_factor*sum(E0[i_restrict,])
    #  Impose export restrictions by reducing exports
    E0[i_restrict,]<- (1. - reduce_factor)*E0[i_restrict,]
    
}

# Step 8: Update food balance variables ----
#  Update state variables for trade_dat after trade restrictions
trade_dat$R <- Rcurrent
trade_dat$E <- E0

ImportsFinal<-colSums(trade_dat$E)
ExportsFinal<-rowSums(trade_dat$E)

I_final_out[,1]<-colSums(trade_dat$E)
E_final_out[,1]<-rowSums(trade_dat$E)

#Update results
results_FSCstatic <- list(P = trade_dat$P + Shocks$dP, R = Rcurrent, C1=trade_dat$C1,E=E0)
# Store outputs of interest from simulations ====
#   Output: 1D arrays
i=1
Pout[, i+1]    <- as.numeric(unlist(results_FSCstatic$P))
Rout[, i+1]    <- as.numeric(unlist(results_FSCstatic$R))

P_final_out[, i]    <- as.numeric(unlist(results_FSCstatic$P))
R_final_out[, i]    <- as.numeric(unlist(results_FSCstatic$R))


# Impaired supply (= perturbed supply - baseline supply)
ImpairedSupply[, i] <-  as.numeric(unlist(results_FSCstatic$C1)) - Supply_baseline 
ImpairedSupplyToReserves[,i] <- ImpairedSupply / R_initial_out


# Step 9: Compute network statistics
# Bilateral Export Matrix (2D array)
Eout[, , 2]  <- as.numeric(unlist(results_FSCstatic$E))
colnames(Eout)  <- InputFSC$iso3
rownames(Eout)  <- InputFSC$iso3
num_countries <- dim(Eout)[2]


# Network metrics
#   Baseline-state network statistics
G  <- Eout[, , 1]
Gnet <- graph_from_adjacency_matrix(G, mode = "directed", weighted = TRUE)

# Total degree
Gdeg_total <- degree(Gnet, mode = "all")
Gdeg_total_df <- data.frame(Gdeg_total)
Gdeg_total_df <- tibble::rownames_to_column(Gdeg_total_df, "iso3")
Gdeg_total_df <- Gdeg_total_df %>%  add_column(Year = baseyear)
Gdeg_total_df <- plyr::join(country_list,Gdeg_total_df, by = 'iso3')  
Gdeg_total_df <-arrange(Gdeg_total_df,iso3)      

# Out degree
Gdeg_out <- degree(Gnet, mode = "out")
Gdeg_out_df <- data.frame(Gdeg_out)
Gdeg_out_df <- tibble::rownames_to_column(Gdeg_out_df, "iso3")
Gdeg_out_df <- Gdeg_out_df %>%  add_column(Year = baseyear)
Gdeg_out_df <- plyr::join(country_list,Gdeg_out_df, by = 'iso3')  
Gdeg_out_df <-arrange(Gdeg_out_df,iso3) 

# In degree
Gdeg_in <- degree(Gnet, mode = "in")
Gdeg_in_df <- data.frame(Gdeg_in)
Gdeg_in_df <- tibble::rownames_to_column(Gdeg_in_df, "iso3")
Gdeg_in_df <- Gdeg_in_df %>%  add_column(Year = baseyear)
Gdeg_in_df <- plyr::join(country_list,Gdeg_in_df, by = 'iso3')  
Gdeg_in_df <-arrange(Gdeg_in_df,iso3) 

# Total strength
Gstrength_total <- strength(Gnet)
Gstrength_total_df <- data.frame(Gstrength_total)
Gstrength_total_df <- tibble::rownames_to_column(Gstrength_total_df, "iso3")
Gstrength_total_df <- Gstrength_total_df %>%  add_column(Year = baseyear)
Gstrength_total_df <- plyr::join(country_list,Gstrength_total_df, by = 'iso3')  
Gstrength_total_df <-arrange(Gstrength_total_df,iso3) 

# Betweenness centrality
Gbtw <- betweenness(Gnet,directed = TRUE, weights = NULL)
Gbtw <-Gbtw/((num_countries-1)*(num_countries-2)/2)
Gbtw_df <- data.frame(Gbtw)
Gbtw_df <- tibble::rownames_to_column(Gbtw_df, "iso3")
Gbtw_df <- Gbtw_df %>%  add_column(Year = baseyear)
Gbtw_df <- plyr::join(country_list,Gbtw_df, by = 'iso3')  
Gbtw_df <-arrange(Gbtw_df,iso3) 

# Eigen centrality
Geigencentral <- eigen_centrality(Gnet)$vector
Geigencentral_df <- data.frame(Geigencentral)
Geigencentral_df <- tibble::rownames_to_column(Geigencentral_df, "iso3")
Geigencentral_df <- Geigencentral_df %>%  add_column(Year = baseyear)
Geigencentral_df <- plyr::join(country_list,Geigencentral_df, by = 'iso3')  
Geigencentral_df <-arrange(Geigencentral_df,iso3) 

# Edge density
Gedgedensity<-edge_density(Gnet, loops=F)

# Hub score
hs <- hub_score(Gnet)$vector
hs_df <- data.frame(hs)
hs_df <- tibble::rownames_to_column(hs_df, "iso3")
hs_df <- hs_df %>%  add_column(Year = baseyear)
hs_df <- plyr::join(country_list,hs_df, by = 'iso3')  
hs_df <-arrange(hs_df,iso3) 

# Perturbed-state network statistics
Gfinal  <- Eout[, , 2]
Gnet_final  <- graph_from_adjacency_matrix(Gfinal, mode = "directed", weighted = TRUE)

# Total degree
Gdeg_total_final <- degree(Gnet_final, mode = "all")
Gdeg_total_df_final <- data.frame(Gdeg_total_final)
Gdeg_total_df_final <- tibble::rownames_to_column(Gdeg_total_df_final, "iso3")
Gdeg_total_df_final <- Gdeg_total_df_final %>%  add_column(Year = baseyear+1)     
Gdeg_total_df_final <- Gdeg_total_df_final %>% rename(Gdeg_total = Gdeg_total_final)
Gdeg_total_df_final <- plyr::join(country_list,Gdeg_total_df_final, by = 'iso3')  
Gdeg_total_df_final <-arrange(Gdeg_total_df_final,iso3)

# Out degree
Gdeg_out_final <- degree(Gnet_final, mode = "out")
Gdeg_out_df_final <- data.frame(Gdeg_out_final)
Gdeg_out_df_final <- tibble::rownames_to_column(Gdeg_out_df_final, "iso3")
Gdeg_out_df_final <- Gdeg_out_df_final %>%  add_column(Year = baseyear+1)   
Gdeg_out_df_final <- Gdeg_out_df_final %>% rename(Gdeg_out = Gdeg_out_final)
Gdeg_out_df_final <- plyr::join(country_list,Gdeg_out_df_final, by = 'iso3')  
Gdeg_out_df_final <-arrange(Gdeg_out_df_final,iso3)

# In degree
Gdeg_in_final <- degree(Gnet_final, mode = "in")
Gdeg_in_df_final <- data.frame(Gdeg_in_final)
Gdeg_in_df_final <- tibble::rownames_to_column(Gdeg_in_df_final, "iso3")
Gdeg_in_df_final <- Gdeg_in_df_final %>%  add_column(Year = baseyear+1)   
Gdeg_in_df_final <- Gdeg_in_df_final %>% rename(Gdeg_in = Gdeg_in_final)
Gdeg_in_df_final <- plyr::join(country_list,Gdeg_in_df_final, by = 'iso3')  
Gdeg_in_df_final <-arrange(Gdeg_in_df_final,iso3)

# Total strength
Gstrength_total_final <- strength(Gnet_final)
Gstrength_total_df_final <- data.frame(Gstrength_total_final)
Gstrength_total_df_final <- tibble::rownames_to_column(Gstrength_total_df_final, "iso3")
Gstrength_total_df_final <- Gstrength_total_df_final %>%  add_column(Year = baseyear+1)   
Gstrength_total_df_final <- Gstrength_total_df_final %>% rename(Gstrength_total = Gstrength_total_final)
Gstrength_total_df_final <- plyr::join(country_list,Gstrength_total_df_final, by = 'iso3')  
Gstrength_total_df_final <-arrange(Gstrength_total_df_final,iso3)

# Betweenness centrality
Gbtw_final <- betweenness(Gnet_final,directed = TRUE, weights = NULL)
Gbtw_final <-Gbtw_final/((num_countries-1)*(num_countries-2)/2)
Gbtw_df_final <- data.frame(Gbtw_final)
Gbtw_df_final <- tibble::rownames_to_column(Gbtw_df_final, "iso3")
Gbtw_df_final <- Gbtw_df_final %>%  add_column(Year = baseyear+1)   
Gbtw_df_final <- Gbtw_df_final %>% rename(Gbtw = Gbtw_final)
Gbtw_df_final <- plyr::join(country_list,Gbtw_df_final, by = 'iso3')  
Gbtw_df_final <-arrange(Gbtw_df_final,iso3)

# Eigen centrality
Geigencentral_final <- eigen_centrality(Gnet_final)$vector
Geigencentral_df_final <- data.frame(Geigencentral_final)
Geigencentral_df_final <- tibble::rownames_to_column(Geigencentral_df_final, "iso3")
Geigencentral_df_final <- Geigencentral_df_final %>%  add_column(Year = baseyear+1)   
Geigencentral_df_final <- Geigencentral_df_final %>% rename(Geigencentral = Geigencentral_final)
Geigencentral_df_final <- plyr::join(country_list,Geigencentral_df_final, by = 'iso3')  
Geigencentral_df_final <-arrange(Geigencentral_df_final,iso3)

# Hub score
hs_final <- hub_score(Gnet_final)$vector
hs_df_final <- data.frame(hs_final)
hs_df_final <- tibble::rownames_to_column(hs_df_final, "iso3")
hs_df_final <- hs_df_final %>%  add_column(Year = baseyear+1)
hs_df_final <- hs_df_final %>% rename(hs = hs_final)
hs_df_final <- plyr::join(country_list,hs_df_final, by = 'iso3')  
hs_df_final <-arrange(hs_df_final,iso3)

# Merge
Gdeg_total_all<- bind_rows(Gdeg_total_df,Gdeg_total_df_final) 
Gdeg_out_all<- bind_rows(Gdeg_out_df,Gdeg_out_df_final) 
Gdeg_in_all<- bind_rows(Gdeg_in_df,Gdeg_in_df_final)

Gstrength_total_all<- bind_rows(Gstrength_total_df,Gstrength_total_df_final)  
Gbtw_all<- bind_rows(Gbtw_df,Gbtw_df_final) 
Geigencentral_all<- bind_rows(Geigencentral_df,Geigencentral_df_final) 
hs_all<- bind_rows(hs_df,hs_df_final) 

# Step 10: Collect, reformat and save output data ----
# Production
colnames(P_initial_out)  <- column_names1
rownames(P_initial_out)  <- InputFSC$iso3
P_initial_out_df <- data.frame(P_initial_out)
P_initial_out_df <- tibble::rownames_to_column(P_initial_out_df, "iso3")
#~ P_initial_out_df <- merge(InputFSC[, c("iso3", "Country.x")], P_initial_out_df, by = "iso3")
P_initial_out_df <- merge(InputFSC[, c("iso3", "Country")], P_initial_out_df, by = "iso3")
# combine the year columns into a single column with separate rows for each year; assign to new vector
P_initial_out_df <- gather(P_initial_out_df, Year, Value, -iso3, -Country)
# remove preceeding X character for Year column aand convert to numeric
P_initial_out_df$Year <- as.numeric(gsub("[a-zA-Z ]", "", P_initial_out_df$Year))
P_initial_out_df$Year <-P_initial_out_df$Year + baseyear


colnames(P_final_out)  <- column_names2
rownames(P_final_out)  <- InputFSC$iso3
P_final_out_df <- data.frame(P_final_out)
P_final_out_df <- tibble::rownames_to_column(P_final_out_df, "iso3")
P_final_out_df <- merge(InputFSC[, c("iso3", "Country")], P_final_out_df, by = "iso3")
# combine the year columns into a single column with separate rows for each year; assign to new vector
P_final_out_df <- gather(P_final_out_df, Year, Value, -iso3, -Country)
# remove preceeding X character for Year column aand convert to numeric
P_final_out_df$Year <- as.numeric(gsub("[a-zA-Z ]", "", P_final_out_df$Year))
P_final_out_df$Year <-P_final_out_df$Year + baseyear

# Put values (for all time points) into a single column with a corresponding time column
P_all<- bind_rows(P_initial_out_df,P_final_out_df) 
names(P_all)[names(P_all)=="Value"] <- "Production"
P_all$Year <- as.numeric(gsub("[a-zA-Z ]", "", P_all$Year))


# Exports
colnames(E_initial_out)  <- column_names1
rownames(E_initial_out)  <- InputFSC$iso3
E_initial_out_df <- data.frame(E_initial_out)
E_initial_out_df <- tibble::rownames_to_column(E_initial_out_df, "iso3")
#~ E_initial_out_df <- merge(InputFSC[, c("iso3", "Country.x")], E_initial_out_df, by = "iso3")
E_initial_out_df <- merge(InputFSC[, c("iso3", "Country")], E_initial_out_df, by = "iso3")
# combine the year columns into a single column with separate rows for each year; assign to new vector
E_initial_out_df <- gather(E_initial_out_df, Year, Value, -iso3, -Country)
# remove preceeding X character for Year column aand convert to numeric
E_initial_out_df$Year <- as.numeric(gsub("[a-zA-Z ]", "", E_initial_out_df$Year))

colnames(E_final_out)  <- column_names2
rownames(E_final_out)  <- InputFSC$iso3
E_final_out_df <- data.frame(E_final_out)
E_final_out_df <- tibble::rownames_to_column(E_final_out_df, "iso3")
#~ E_final_out_df <- merge(InputFSC[, c("iso3", "Country.x")], E_final_out_df, by = "iso3")
E_final_out_df <- merge(InputFSC[, c("iso3", "Country")], E_final_out_df, by = "iso3")
# combine the year columns into a single column with separate rows for each year; assign to new vector
E_final_out_df <- gather(E_final_out_df, Year, Value, -iso3, -Country)
# remove preceeding X character for Year column aand convert to numeric
E_final_out_df$Year <- as.numeric(gsub("[a-zA-Z ]", "", E_final_out_df$Year))

# Put values (for all time points) into a single column with a corresponding time column
E_all<- bind_rows(E_initial_out_df,E_final_out_df) 
names(E_all)[names(E_all)=="Value"] <- "Exports"
E_all$Year <- as.numeric(gsub("[a-zA-Z ]", "", E_all$Year))


# Imports
colnames(I_initial_out)  <- column_names1
rownames(I_initial_out)  <- InputFSC$iso3
I_initial_out_df <- data.frame(I_initial_out)
I_initial_out_df <- tibble::rownames_to_column(I_initial_out_df, "iso3")
#~ I_initial_out_df <- merge(InputFSC[, c("iso3", "Country.x")], I_initial_out_df, by = "iso3")
I_initial_out_df <- merge(InputFSC[, c("iso3", "Country")], I_initial_out_df, by = "iso3")
# combine the year columns into a single column with separate rows for each year; assign to new vector
I_initial_out_df <- gather(I_initial_out_df, Year, Value, -iso3, -Country)
# remove preceeding X character for Year column aand convert to numeric
I_initial_out_df$Year <- as.numeric(gsub("[a-zA-Z ]", "", I_initial_out_df$Year))

colnames(I_final_out)  <- column_names2
rownames(I_final_out)  <- InputFSC$iso3
I_final_out_df <- data.frame(I_final_out)
I_final_out_df <- tibble::rownames_to_column(I_final_out_df, "iso3")
#~ I_final_out_df <- merge(InputFSC[, c("iso3", "Country.x")], I_final_out_df, by = "iso3")
I_final_out_df <- merge(InputFSC[, c("iso3", "Country")], I_final_out_df, by = "iso3")
# combine the year columns into a single column with separate rows for each year; assign to new vector
I_final_out_df <- gather(I_final_out_df, Year, Value, -iso3, -Country)
# remove preceeding X character for Year column aand convert to numeric
I_final_out_df$Year <- as.numeric(gsub("[a-zA-Z ]", "", I_final_out_df$Year))

# Put values (for all time points) into a single column with a corresponding time column
I_all<- bind_rows(I_initial_out_df,I_final_out_df) 
names(I_all)[names(I_all)=="Value"] <- "Imports"
I_all$Year <- as.numeric(gsub("[a-zA-Z ]", "", I_all$Year))


# Reserves
colnames(R_initial_out)  <- column_names1
rownames(R_initial_out)  <- InputFSC$iso3
R_initial_out_df <- data.frame(R_initial_out)
R_initial_out_df <- tibble::rownames_to_column(R_initial_out_df, "iso3")
R_initial_out_df <- merge(InputFSC[, c("iso3", "Country")], R_initial_out_df, by = "iso3")
# combine the year columns into a single column with separate rows for each year; assign to new vector
R_initial_out_df <- gather(R_initial_out_df, Year, Value, -iso3, -Country)
# remove preceeding X character for Year column aand convert to numeric
R_initial_out_df$Year <- as.numeric(gsub("[a-zA-Z ]", "", R_initial_out_df$Year))

colnames(R_final_out)  <- column_names2
rownames(R_final_out)  <- InputFSC$iso3
R_final_out_df <- data.frame(R_final_out)
R_final_out_df <- tibble::rownames_to_column(R_final_out_df, "iso3")
R_final_out_df <- merge(InputFSC[, c("iso3", "Country")], R_final_out_df, by = "iso3")
# combine the year columns into a single column with separate rows for each year; assign to new vector
R_final_out_df <- gather(R_final_out_df, Year, Value, -iso3, -Country)
# remove preceeding X character for Year column aand convert to numeric
R_final_out_df$Year <- as.numeric(gsub("[a-zA-Z ]", "", R_final_out_df$Year))

# Put values (for all time points) into a single column with a corresponding time column
R_all<- bind_rows(R_initial_out_df,R_final_out_df) 
names(R_all)[names(R_all)=="Value"] <- "Reserves"
R_all$Year <- as.numeric(gsub("[a-zA-Z ]", "", R_all$Year))

# Impaired supply is calculated as the difference between the supply in an unperturbed baseline scenario 
# and the perturbed scenario. It describes the supply gap a country has to close by either tapping into its 
# reserve, placing additional orders at the world market or reducing consumption. 
colnames(ImpairedSupply)  <- column_names2
rownames(ImpairedSupply)  <- InputFSC$iso3
ImpairedSupply_df <- data.frame(ImpairedSupply)
ImpairedSupply_df <- tibble::rownames_to_column(ImpairedSupply_df, "iso3")
ImpairedSupply_df <-
  merge(InputFSC[, c("iso3", "Country")], ImpairedSupply_df, by = "iso3")
# combine the year columns into a single column with separate rows for each year; assign to new vector
ImpairedSupply_df <- gather(ImpairedSupply_df, Year, Value, -iso3, -Country)
# remove preceeding X character for Year column aand convert to numeric
ImpairedSupply_df$Year <- as.numeric(gsub("[a-zA-Z ]", "", ImpairedSupply_df$Year))

# The ratio of impaired supply and reserves additionally therefore provides a proxy on how dependent a country 
# is on foreign supplies to mitigate the shortage. 
colnames(ImpairedSupplyToReserves)  <- column_names2
rownames(ImpairedSupplyToReserves)  <- InputFSC$iso3
ImpairedSupplyToReserves_df <- data.frame(ImpairedSupplyToReserves)
ImpairedSupplyToReserves_df <- tibble::rownames_to_column(ImpairedSupplyToReserves_df, "iso3")
ImpairedSupplyToReserves_df <-
  merge(InputFSC[, c("iso3", "Country")], ImpairedSupplyToReserves_df, by = "iso3")
# combine the year columns into a single column with separate rows for each year; assign to new vector
ImpairedSupplyToReserves_df <- gather(ImpairedSupplyToReserves_df, Year, Value, -iso3, -Country)
# remove preceeding X character for Year column and convert to numeric
ImpairedSupplyToReserves_df$Year <- as.numeric(gsub("[a-zA-Z ]", "", ImpairedSupplyToReserves_df$Year))


## Combine output into single file
outputFSC <- P_all
outputFSC <- bind_cols(outputFSC,I_all[, c("Imports"),drop=FALSE])
outputFSC <- bind_cols(outputFSC,E_all[, c("Exports"),drop=FALSE])
outputFSC <- bind_cols(outputFSC,R_all[, c("Reserves"),drop=FALSE])

outputFSC <- bind_cols(outputFSC,Gdeg_total_all[, c("Gdeg_total"),drop=FALSE])
outputFSC <- bind_cols(outputFSC,Gdeg_out_all[, c("Gdeg_out"),drop=FALSE])
outputFSC <- bind_cols(outputFSC,Gdeg_in_all[, c("Gdeg_in"),drop=FALSE])

outputFSC <- bind_cols(outputFSC,Gstrength_total_all[, c("Gstrength_total"),drop=FALSE])
outputFSC <- bind_cols(outputFSC,Gbtw_all[, c("Gbtw"),drop=FALSE])
outputFSC <- bind_cols(outputFSC,Geigencentral_all[, c("Geigencentral"),drop=FALSE])
outputFSC <- bind_cols(outputFSC,hs_all[, c("hs"),drop=FALSE])

## Save as CSV
# output for Dojo
write.csv(outputFSC, paste0(working_directory,"outputs/","outputFSC.csv"), row.names = FALSE)
write.csv(ImpairedSupply_df, paste0(working_directory,"outputs/","ImpairedSupply.csv"), row.names = FALSE)
write.csv(ImpairedSupplyToReserves_df, paste0(working_directory,"outputs/","ImpairedSupplyToReserves.csv"), row.names = FALSE)
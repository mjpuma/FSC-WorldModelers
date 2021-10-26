## Main script for the Food Shock Cascade (FSC) Model
install.packages("dplyr")whe
install.packages("tidyr")
# Load FSC functions
library(dplyr, warn.conflicts = FALSE)
library(tidyr, warn.conflicts = FALSE)
library(stringr)

# Step 0: Setup ----
# Default 1 year run for static model
years <- 1 #Don't Change
# Create column names for output files
column_names = c('0', '1')
column_names2 = c('1')
column_names1 = c('0')

# Create output directory if needed
if (dir.exists("outputs") == FALSE) {
  dir.create("outputs")
}

# Step 1: Specify scenario ----
# Parse arguments ====
args <- commandArgs(trailingOnly = TRUE)
working_directory <- c(args[1])             # Scenario from the FSC Scenario Library
crop <- c(args[2])             # Scenario from the FSC Scenario Library
anomaly_factor <- c(as.numeric(args[3]))             # Scenario from the FSC Scenario Library
restriction_intensity <- c(as.numeric(args[4]))             # Scenario from the FSC Scenario Library


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
Cout <-  array(0, c(nrow(country_list), length(years) + 1))
Eout <-  array(0, c(nrow(country_list), nrow(country_list), length(years) + 1))

shortageout <- array(0, c(nrow(country_list), length(years)))
C1_C0out <- array(0, c(nrow(country_list), length(years)))
C2out <- array(0, c(nrow(country_list), length(years)))
C0out <- array(0, c(nrow(country_list), length(years)))
dR_C0out <- array(0, c(nrow(country_list), length(years)))


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
# Set Reserves and add POSTIVE anomalies to reserves ====
InputFSC <- plyr::join(Shocks,InputFSC, by = 'iso3')




# Add positive anomalies to reserves ====
# Add positive anomalies to reserves ====
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
C0_initial <- trade_dat$C
Cout[, 1] <- C0_initial
# Initial shortage is 0
trade_dat$shortage <- rep(0, trade_dat$nc)

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

# Domestic Supply after production declines and trade restrictions
trade_dat$C2 <- trade_dat$P + Shocks$dP + colSums(trade_dat$E) - rowSums(trade_dat$E)

ImportsFinal<-colSums(trade_dat$E)
ExportsFinal<-rowSums(trade_dat$E)

I_final_out[,1]<-colSums(trade_dat$E)
E_final_out[,1]<-rowSums(trade_dat$E)

#Update results
results_FSCstatic <- list(P = trade_dat$P + Shocks$dP, R = Rcurrent, C1=trade_dat$C1, C2=trade_dat$C2, shortage =trade_dat$shortage,E=E0)
# Store outputs of interest from simulations ====
#   Output: 1D arrays
i=1
Pout[, i+1]    <- as.numeric(unlist(results_FSCstatic$P))
Rout[, i+1]    <- as.numeric(unlist(results_FSCstatic$R))
Cout[, i+1]    <- as.numeric(unlist(results_FSCstatic$C1))
shortageout[, i] <- as.numeric(unlist(results_FSCstatic$shortage))


P_final_out[, i]    <- as.numeric(unlist(results_FSCstatic$P))
R_final_out[, i]    <- as.numeric(unlist(results_FSCstatic$R))



# Consumption relative to initial consumption
C1_C0out[, i] <- as.numeric(unlist(results_FSCstatic$C1)) / C0_initial
#~ C2_C0out[, i] <- as.numeric(unlist(results_FSCstatic$C2)) / C0_initial
C2out[, i] <- as.numeric(unlist(results_FSCstatic$C2)) 
C0out[,i] <- C0_initial
#     change in reserves relative to initial consumption
#dR_C0out[, i] <- results_FSC$dR / C0_initial
  
# Step 9: Collect, reformat and save output data ----
# Production
colnames(Pout)  <- column_names
rownames(Pout)  <- InputFSC$iso3

Pout_df <- data.frame(Pout)

Pout_df <- tibble::rownames_to_column(Pout_df, "iso3")


#~ Pout_df <- merge(InputFSC[, c("iso3", "Country.x")], Pout_df, by = "iso3")
Pout_df <- merge(InputFSC[, c("iso3", "Country")], Pout_df, by = "iso3")

# combine the year columns into a single column with separate rows for each year; assign to new vector
Pout_df <- gather(Pout_df, Year, Value, -iso3, -Country)
# remove preceeding X character for Year column aand convert to numeric
Pout_df$Year <- as.numeric(gsub("[a-zA-Z ]", "", Pout_df$Year))



colnames(P_final_out)  <- column_names2

rownames(P_final_out)  <- InputFSC$iso3
P_final_out_df <- data.frame(P_final_out)

P_final_out_df <- tibble::rownames_to_column(P_final_out_df, "iso3")
P_final_out_df <- merge(InputFSC[, c("iso3", "Country")], P_final_out_df, by = "iso3")
# combine the year columns into a single column with separate rows for each year; assign to new vector
P_final_out_df <- gather(P_final_out_df, Year, Value, -iso3, -Country)
# remove preceeding X character for Year column aand convert to numeric
P_final_out_df$Year <- as.numeric(gsub("[a-zA-Z ]", "", P_final_out_df$Year))



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

# Exports

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


# Imports

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

# Reserves
colnames(Rout)  <- column_names
rownames(Rout)  <- InputFSC$iso3
Rout_df <- data.frame(Rout)
Rout_df <- tibble::rownames_to_column(Rout_df, "iso3")
#~ Rout_df <- merge(InputFSC[, c("iso3", "Country.x")], Rout_df, by = "iso3")
Rout_df <- merge(InputFSC[, c("iso3", "Country")], Rout_df, by = "iso3")
# combine the year columns into a single column with separate rows for each year; assign to new vector
Rout_df <- gather(Rout_df, Year, Value, -iso3, -Country)
# remove preceeding X character for Year column aand convert to numeric
Rout_df$Year <- as.numeric(gsub("[a-zA-Z ]", "", Rout_df$Year))




colnames(R_final_out)  <- column_names2
rownames(R_final_out)  <- InputFSC$iso3
R_final_out_df <- data.frame(R_final_out)
R_final_out_df <- tibble::rownames_to_column(R_final_out_df, "iso3")
R_final_out_df <- merge(InputFSC[, c("iso3", "Country")], R_final_out_df, by = "iso3")
# combine the year columns into a single column with separate rows for each year; assign to new vector
R_final_out_df <- gather(R_final_out_df, Year, Value, -iso3, -Country)
# remove preceeding X character for Year column aand convert to numeric
R_final_out_df$Year <- as.numeric(gsub("[a-zA-Z ]", "", R_final_out_df$Year))

colnames(R_initial_out)  <- column_names1
rownames(R_initial_out)  <- InputFSC$iso3
R_initial_out_df <- data.frame(R_initial_out)
R_initial_out_df <- tibble::rownames_to_column(R_initial_out_df, "iso3")
R_initial_out_df <- merge(InputFSC[, c("iso3", "Country")], R_initial_out_df, by = "iso3")
# combine the year columns into a single column with separate rows for each year; assign to new vector
R_initial_out_df <- gather(R_initial_out_df, Year, Value, -iso3, -Country)
# remove preceeding X character for Year column aand convert to numeric
R_initial_out_df$Year <- as.numeric(gsub("[a-zA-Z ]", "", R_initial_out_df$Year))



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
  merge(InputFSC[, c("iso3", "Country")], shortageout_df, by = "iso3")
# combine the year columns into a single column with separate rows for each year; assign to new vector
shortageout_df <- gather(shortageout_df, Year, Value, -iso3, -Country)
# remove preceeding X character for Year column aand convert to numeric
shortageout_df$Year <-
  as.numeric(gsub("[a-zA-Z ]", "", shortageout_df$Year))

# Consumption to C0
colnames(C1_C0out)  <- column_names2
rownames(C1_C0out)  <- InputFSC$iso3
C1_C0out_df <- data.frame(C1_C0out)
C1_C0out_df <- tibble::rownames_to_column(C1_C0out_df, "iso3")
C1_C0out_df <-
  merge(InputFSC[, c("iso3", "Country")], C1_C0out_df, by = "iso3")
# combine the year columns into a single column with separate rows for each year; assign to new vector
C1_C0out_df <- gather(C1_C0out_df, Year, Value, -iso3, -Country)
# remove preceeding X character for Year column aand convert to numeric
C1_C0out_df$Year <- as.numeric(gsub("[a-zA-Z ]", "", C1_C0out_df$Year))

#Cosnumption/Supply at the end
colnames(C2out)  <- column_names2
rownames(C2out)  <- InputFSC$iso3
C2out_df <- data.frame(C2out)
C2out_df <- tibble::rownames_to_column(C2out_df, "iso3")
C2out_df <-
  merge(InputFSC[, c("iso3", "Country")], C2out_df, by = "iso3")
# combine the year columns into a single column with separate rows for each year; assign to new vector
C2out_df <- gather(C2out_df, Year, Value, -iso3, -Country)
# remove preceeding X character for Year column and convert to numeric
C2out_df$Year <- as.numeric(gsub("[a-zA-Z ]", "", C2out_df$Year))

#Initial Cosnumption/Supply at the end
colnames(C0out)  <- column_names2
rownames(C0out)  <- InputFSC$iso3
C0out_df <- data.frame(C0out)
C0out_df <- tibble::rownames_to_column(C0out_df, "iso3")
C0out_df <-
  merge(InputFSC[, c("iso3", "Country")], C0out_df, by = "iso3")
# combine the year columns into a single column with separate rows for each year; assign to new vector
C0out_df <- gather(C0out_df, Year, Value, -iso3, -Country)
# remove preceeding X character for Year column and convert to numeric
C0out_df$Year <- as.numeric(gsub("[a-zA-Z ]", "", C0out_df$Year))

## Save as CSV
write.csv(P_initial_out_df, paste0(working_directory,"outputs/",runname,"production_initial.csv"), row.names = FALSE)
write.csv(P_final_out_df, paste0(working_directory,"outputs/",runname,"production_final.csv"), row.names = FALSE)
write.csv(R_initial_out_df, paste0(working_directory,"outputs/",runname,"reserve_initial.csv"), row.names = FALSE)
write.csv(R_final_out_df, paste0(working_directory,"outputs/",runname,"reserve_final.csv"), row.names = FALSE)

write.csv(E_initial_out_df, paste0(working_directory,"outputs/",runname,"export_initial.csv"), row.names = FALSE)
write.csv(E_final_out_df, paste0(working_directory,"outputs/",runname,"export_final.csv"), row.names = FALSE)
write.csv(I_initial_out_df, paste0(working_directory,"outputs/",runname,"import_initial.csv"), row.names = FALSE)
write.csv(I_final_out_df, paste0(working_directory,"outputs/",runname,"import_final.csv"), row.names = FALSE)

write.csv(C2out_df, paste0(working_directory,"outputs/",runname,"supply_final.csv"), row.names = FALSE)
write.csv(C0out_df, paste0(working_directory,"outputs/",runname,"supply_initial.csv"), row.names = FALSE)

# Save Exports as R data file
saveRDS(Eout, file = paste0(working_directory,"outputs/", runname,"ExportStatic.rds"))

#######################
### LOAD TRADE DATA ###
#######################

# Specify arguments
years <- 2014
country <- "USA"
production_decrease <- 0.4
fractional_reserve_access <- 0.5
output_file_name <- "USA_shock"

## Command line version: Parse arguments
# args <- commandArgs(trailingOnly = TRUE)
# years <- c(as.numeric(args[1]))
# country <- args[2]
# production_decrease <- as.numeric(args[3])
# fractional_reserve_access <- as.numeric(args[4])
# output_file_name <- args[5]

# Create output directory if needed
if (dir.exists("outputs") == FALSE) {
  dir.create("outputs")
}

# Load trade data
setwd("~/GitHub_mjpuma/FSC-WorldModelers/")            ###Remove before commit
source("main/component_funcs.R")
source("main/sim_funcs.R")
library(dplyr, warn.conflicts = FALSE)

# Set year range for production, trade, and reserves data
yr_range <- 2012:2016

Anomalies = pd.DataFrame(index=np.arange(length(yr_range)), columns=np.arange(1))
for (i in 1:length(Anomalies)) {
  Anomalies[i] <-production_decrease
}

# Load ancillary data
# 1)  Commodity list 
commodities<-read.csv("ancillary/cropcommodity_list.csv")
# 2) Load country list
country_list <- read.csv("ancillary/country_list195.csv")

## Load production, trade(export matrix), stocks data
load("inputs_processed/P0.Rdata") #Production
load("inputs_processed/E0.RData") #Export Matrix
load("inputs_processed/R0.RData") #Reserves (a.k.a. Stocks)

for (i in 1:length(years)) {
  
  ## Create 'Shocks' dataframe
  Shocks <-
    merge(
      country_list,
      Anomalies[i],
      by.x = 'iso3',
      by.y = 'X',
      all.x = TRUE,
      all.y = FALSE
    )
  Shocks[is.na(Shocks)] <- 0
  colnames(Shocks)[4] <- "singleyr"
  
  ## Separate fractional gains and losses in production
  Shocks$FracGain <- ifelse(Shocks$singleyr > 0, Shocks$singleyr, 0)
  Shocks$FracLoss <-
    ifelse(Shocks$singleyr > 0, 0, (Shocks$singleyr * -1))
  P <- data.frame(P0 = P0, iso3 = names(P0))
  Shocks <- merge(Shocks, P, by = "iso3")
  Shocks$dP <- (-Shocks$FracLoss) * (Shocks$P0)
  Shocks <- Shocks[order(Shocks$FAO), ]
  
  ## Create 'Reserves' dataframe
  Reserves <- data_frame(iso3 = names(R0), R0 = R0)
  Reserves <-
    merge(Shocks,
          Reserves,
          by = 'iso3',
          all.x = TRUE,
          all.y = FALSE)
  Reserves <- Reserves[order(Reserves$FAO), ]
  
  ## Add positive anomalies to reserves
  Reserves$R1 <- (Reserves$P0 * Reserves$FracGain) + Reserves$R0
  identical(Reserves$R0[Reserves$Y1Inc == 0], Reserves$R1[Reserves$FracGain ==
                                                            0]) ## R0=R1 where no production increases occurred
  R1 <- (Reserves$R1)
  names(R1) <- Reserves$iso3
  
  ## Convert negative production shocks from dataframe to list
  shockintensities <- Shocks$FracLoss
  names(shockintensities) <- Shocks$iso3
  SingleyrLoss <- list(shockintensities)
  
  ## Run Marchand et al (2016) version of Food Shocks Cascade (FSC) model
  Results_raw <- sim_mc_multi(
    SingleyrLoss, 
    P0, 
    R1,
    E0,
    cfrac = 0.1,
    asym = T,
    kmax = 1000,
    amin = 1E-5
  )
  
  ## Store output in dataframe
  
  # Create dataframe for output if i = 1
  if (i == 1) {
    FSCResults <- data.frame(iso3 = names(Results_raw$P0), 
                             dR = 1:length(years),
                             R1 = R1)
  }
  text <- paste("dR", i)
  FSCResults$dR[[text]] <- ifelse(FSCResults$dR == i, 1, 0)
  FSCResults <- data.frame(dR = Results_raw$dR)
}












# Load selected year data with moving average of X years on either side
trade_dat <- lapply(years, get_trade_data, mov_avg = 1, 
                    prod_trade_file = "cereals_prod_trade.RData", 
                    stocks_file = "cereals_stocks.RData")
names(trade_dat) <- years

####trade_st <- bind_rows(lapply(trade_dat, get_trade_stats_sum), .id = "year")

years_str <- toString(years)
P0<-trade_dat[[years_str]]$P0
R0<-trade_dat[[years_str]]$R0
E0<-trade_dat[[years_str]]$E0


###
# Add a few state variables to trade_dat (which initially contains P0, R0, E0)
trade_dat<-list(P = P0, R = R0, E = E0)
trade_dat$nc <- length(trade_dat$P) # number of countries
trade_dat$dR <- rep(0, trade_dat$nc) # change in reserves
trade_dat$C <- get_supply(trade_dat) # consumption initially equal to supply
trade_dat$shortage <- rep(0, trade_dat$nc) # initial shortage is 0

# Set initial shock
dP <- rep(0, trade_dat$nc)
names(dP) <- names(trade_dat$P)
dP["USA"] <- -0.04 * trade_dat$P["USA"] #year1

# Call main simulation function
resY1 <- sim_cascade_v2(trade_datY1, dPY1)

# Calculate outputs of interest from results
dR_C0Y1 <- resY1$dR / trade_datY1$C # change in reserves relative to initial consumption
dC_C0Y1 <- (resY1$C - trade_datY1$C) / trade_datY1$C  # relative change in consumption 
dEY1 <- resY1$E - trade_datY1$E # change in trade


###



# Add a few state variables to trade_dat (which initially contains P0, R0, E0)
setwd(paste(topdir, "/main", sep=""))
source("component_funcs.R")
source("sim_funcs.R")
names(trade_dat) <- c("P", "R", "E")
trade_dat$nc <- length(trade_dat$P) # number of countries
trade_dat$dR <- rep(0, trade_dat$nc) # change in reserves
trade_dat$C <- get_supply(trade_dat) # consumption initially equal to supply
trade_dat$shortage <- rep(0, trade_dat$nc) # initial shortage is 0

##################
# # New version
# source("component_funcs.R")
# source("sim_funcs.R")
# 
# 
# # Add a few state variables to trade_dat (which initially contains P0, R0, E0)
# names(trade_dat) <- c("P", "R", "E")
# trade_dat$nc <- length(trade_dat$P) # number of countries
# trade_dat$dR <- rep(0, trade_dat$nc) # change in reserves
# trade_dat$C <- get_supply(trade_dat) # consumption initially equal to supply
# trade_dat$shortage <- rep(0, trade_dat$nc) # initial shortage is 0
# 



############################
### SINGLE COUNTRY SHOCK ###
############################
single_country <- function(P0, 
                           R0, 
                           E0, 
                           topdir, 
                           country, 
                           production_decrease, 
                           fractional_reserve_access,
                           output_file_name) {
  
  #####shock<-sim_1c(country, production_decrease, P0, fractional_reserve_access*R0, E0)
  shock <- sim_cascade_v2(trade_dat, production_decrease) # original version
  
   
  # # Tests whether output respects equation: S= P + I - E = R + C
  # sim_diag <- sim_diagnostics(shock)
  # cat("Checking whether output respects equation: S= P + I - E = R + C:\n")
  # cat(sim_diag)
  # cat("\n\n")
  # 
  # ### calculates the depth of the single cascade simulation
  # # maximum graph distance from the intital shock country to any country hit by shock
  # casc_depth <- cascade_depth(country,shock$dE)
  # cat("Calculating maximum graph distance from the intital shock country to any country hit by shock:\n")
  # cat(casc_depth)
  # cat("\n\n")
  
  # Write output
  setwd(paste(topdir, "/outputs", sep=""))
  if (dir.exists(output_file_name) == FALSE) {
    dir.create(output_file_name)
  }
  setwd(paste(topdir, "/outputs/", output_file_name, sep=""))
  
  # Write shock results
  # Compute Initial Supply
  S0 <- shock$P0 + colSums(shock$E0) - rowSums(shock$E0)
  shock_results <- data.frame(names(shock$dP),
                             shock$P0,
                             shock$dP, 
                             shock$R0, 
                             shock$dR, 
                             shock$dC, 
                             S0=S0)
  colnames(shock_results) <- c("country", "P0", "dP", "R0", "dR", "dC", "S0")
  file_name <- "single_shock_results.csv"
  cat("\nWriting output of: shock_results", sep="")
  write.csv(shock_results, file = file_name, row.names=FALSE)
  
  
  # Write E0
  file_name <- "E0.csv"
  cat("\nWriting output of: E0", sep="")
  write.csv(shock$E0, file = file_name)
  
  # Write dE
  file_name <- "dE.csv"
  cat("\nWriting output of: dE", sep="")
  write.csv(shock$dE, file = file_name)
  
  return(cat("\nOutput files stored to /outputs/", output_file_name, sep=""))
}

###########################
### MULTI COUNTRY SHOCK ###
###########################
multi_country <- function(P0, 
                          R0, 
                          E0, 
                          topdir, 
                          production_decrease,
                          fractional_reserve_access, 
                          output_file_name) {
  AllCountries <- sim_allc(production_decrease, P0, fractional_reserve_access*R0, E0, cfrac = 0.01)
  # Get a list of summary statistics for sim_res_multi
  AllCountriesStats <- get_stats_allc(AllCountries)
  
  # Write output
  setwd(paste(topdir, "/outputs", sep=""))
  stat_names <- names(AllCountriesStats)
  if (dir.exists(output_file_name) == FALSE) {
    dir.create(output_file_name)
  }
  setwd(paste(topdir, "/outputs/", output_file_name, sep=""))
  
  # Write shock results
  shock_results <- data.frame(names(AllCountriesStats$depth_by_sim), 
                              AllCountriesStats$depth_by_sim, 
                              AllCountriesStats$countries_hit_by_sim, 
                              AllCountriesStats$links_hit_by_sim, 
                              AllCountriesStats$total_dC_by_sim, 
                              AllCountriesStats$avg_dS_S0_by_sim_rank)
  colnames(shock_results) <- c("country", 
                               "depth_by_sim", 
                               "countries_hit_by_sim", 
                               "links_hit_by_sim", 
                               "total_dC_by_sim", 
                               "avg_dS_S0_by_sim_rank")
  file_name <- "multi_shock_results.csv"
  cat("\nWriting output of: shock_results", sep="")
  write.csv(shock_results, file = file_name, row.names=FALSE)  
  
  # Write cty results
  # These vectors have length 166 whereas the other vectors have 
  # length 163 so they are written out separately
  # TODO: determine why these vectors have differing lengths
  shock_results <- data.frame(names(AllCountriesStats$hits_by_cty), 
                              AllCountriesStats$hits_by_cty,
                              AllCountriesStats$avg_links_hit_by_cty,
                              AllCountriesStats$avg_dSrel_by_cty_rank)
  colnames(shock_results) <- c("country", 
                               "hits_by_cty",
                               "avg_links_hit_by_cty",
                               "avg_dSrel_by_cty_rank")
  file_name <- "multi_cty_results.csv"
  cat("\nWriting output of: shock_results", sep="")
  write.csv(shock_results, file = file_name, row.names=FALSE)  
  
  # Write delta results
  stat_names <- c("dRrel", "dCrel", "dC_S0", "dS_S0", "hits_by_link")
  for (stat in stat_names) {
    file_name <- paste(stat, ".csv", sep="")
    cat("\nWriting output of: ", stat, sep="")
    output <- AllCountriesStats[[stat]]
    write.csv(output, file = file_name)
  }
  return(cat("\nOutput files stored to /outputs/", output_file_name, sep=""))
}

##########################
### RUN MAIN FUNCTIONS ###
##########################
if (tolower(country) == "all") {
  cat("Country: ", country, "\n", sep=" ")
  cat("Production Decrease: ", production_decrease*100, "%\n", sep="")  
  cat("Fractional Reserve Access: ", fractional_reserve_access*100, "%\n\n", sep="")
  multi_country(P0, 
                R0, 
                E0, 
                topdir, 
                production_decrease,
                fractional_reserve_access, 
                output_file_name)
} else {
  cat("Country: ", country, "\n", sep=" ")
  cat("Production Decrease: ", production_decrease*100, "%\n", sep="")
  cat("Fractional Reserve Access: ", fractional_reserve_access*100, "%\n\n", sep="")
  single_country(P0, 
                 R0, 
                 E0, 
                 topdir, 
                 country, 
                 production_decrease,
                 fractional_reserve_access, 
                 output_file_name)
}
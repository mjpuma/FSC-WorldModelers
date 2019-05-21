#######################
### LOAD TRADE DATA ###
#######################

# Parse arguments
args <- commandArgs(trailingOnly = TRUE)
years <- c(as.numeric(args[1]))
country <- args[2]
production_decrease <- as.numeric(args[3])
fractional_reserve_access <- as.numeric(args[4])
output_file_name <- args[5]

# Create output directory if needed
if (dir.exists("outputs") == FALSE) {
  dir.create("outputs")
}

# Load trade data
topdir <- getwd()
setwd(paste(topdir, "/main", sep=""))
source("CascadeFunctions.R")
setwd(paste(topdir, "/inputs", sep=""))
library(dplyr, warn.conflicts = FALSE)
iso3 <- read.table("ciso3.txt", stringsAsFactors = FALSE) #load country list 
cnames <- iso3[, 2] #select 3 digit ISO character codes 

trade_dat <- lapply(years, get_trade_data, mov_avg = 1, #load selected year data with moving average of 1 years on either side
                    prod_trade_file = "cereals_prod_trade.RData", 
                    stocks_file = "cereals_stocks.RData")
names(trade_dat) <- years

trade_st <- bind_rows(lapply(trade_dat, get_trade_stats_sum), .id = "year")

years_str <- toString(years)
P0<-trade_dat[[years_str]]$P0
R0<-trade_dat[[years_str]]$R0
E0<-trade_dat[[years_str]]$E0

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
  
  shock<-sim_1c(country, production_decrease, P0, fractional_reserve_access*R0, E0)
  
  # Tests whether output respects equation: S= P + I - E = R + C
  sim_diag <- sim_diagnostics(shock)
  cat("Checking whether output respects equation: S= P + I - E = R + C:\n")
  cat(sim_diag)
  cat("\n\n")
  
  ### calculates the depth of the single cascade simulation 
  # maximum graph distance from the intital shock country to any country hit by shock
  casc_depth <- cascade_depth(country,shock$dE)
  cat("Calculating maximum graph distance from the intital shock country to any country hit by shock:\n")
  cat(casc_depth)
  cat("\n\n")
  
  #lapply(shock, function(x) write.table( data.frame(x), 'test.csv'  , append= T, sep=',' ))
  setwd(paste(topdir, "/outputs", sep=""))
  write.csv(shock, file = output_file_name)
  return(cat("Output file stored to /outputs/", output_file_name, sep=""))
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
  if (grepl(".csv", output_file_name) == FALSE) {
    output_file_name <- paste(output_file_name, ".csv", sep="")
  } 
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
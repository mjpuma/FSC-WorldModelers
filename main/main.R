main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  topdir <- getwd()
  setwd(paste(topdir, "/main", sep=""))
  source("CascadeFunctions.R")
  setwd(paste(topdir, "/inputs", sep=""))

  library(dplyr, warn.conflicts = FALSE)
  iso3 <- read.table("ciso3.txt", stringsAsFactors = FALSE) #load country list 
  cnames <- iso3[, 2] #select 3 digit ISO character codes 
  
  years <- c(2005)
  trade_dat <- lapply(years, get_trade_data, mov_avg = 1, #load 2005 data with moving average of 1 years on either side
                      prod_trade_file = "cereals_prod_trade.RData", 
                      stocks_file = "cereals_stocks.RData")
  names(trade_dat) <- years
  
  trade_st <- bind_rows(lapply(trade_dat, get_trade_stats_sum), .id = "year")

  
  P0<-trade_dat$'2005'$P0
  R0<-trade_dat$'2005'$R0
  E0<-trade_dat$'2005'$E0
  
  shock<-sim_1c(args[1], as.numeric(args[2]), P0, as.numeric(args[3])*R0, E0)
  
  # Tests whether output respects equation: S= P + I - E = R + C
  sim_diag <- sim_diagnostics(shock)
  cat("Checking whether output respects equation: S= P + I - E = R + C:\n")
  cat(sim_diag)
  cat("\n\n")
  
  ### calculates the depth of the single cascade simulation 
  # maximum graph distance from the intital shock country to any country hit by shock
  casc_depth <- cascade_depth(args[1],shock$dE)
  cat("Calculating maximum graph distance from the intital shock country to any country hit by shock:\n")
  cat(casc_depth)
  cat("\n\n")
  
  #lapply(shock, function(x) write.table( data.frame(x), 'test.csv'  , append= T, sep=',' ))
  setwd(paste(topdir, "/outputs", sep=""))
  write.csv(shock, file = args[4])
  return(cat("Output file stored to /outputs/", args[4], sep=""))
}

main()
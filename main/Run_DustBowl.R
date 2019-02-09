##  Code to run simulations with the Trade and Food Security cascade model for the Dust Bowl
## Runs sim_cascade
# Note: When a country receives a shock (at any stage) and exhausts its reserves,
# it will both decrease its exports (red arrows) and increase its imports (blue arrows) if possible.
# sim_1c(c_init,a_init, P0, R0, E0)
# where   c_init = name of the initial country that is impacted
#         a_init = fraction of production lost initially by country c_init
#             P0 = initial production vector
#             R0 = initial reserve vector; note: 0.5*R0 effectively means 50% of reserves can be tapped to absorb shock
#             E0 = initial trade matrix
#OPTIONAL: cfrac = fraction of shock absorbed by C before changing trade (i.e. how much consumption would be reduced)
#           asym = TRUE => shocked countries can't increase their exports
#           kmax = max. number of iterations
#           amin = any shock below this value (as fraction of net supply) is negligible
# R Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(R.matlab)

setwd("~/Dropbox/MURI/NuclearWinter")

# Load functions
source("tfs_cascade_funcs.R")
source("tfs_map_funcs.R")
source("res_00_funcs.R")


# Local directory info
data_dir <- "/data"
res_dir <- "/DustBowl"

# Load base trade data
years <- c(2000,2009)
prod_trade_file <-
    file.path(data_dir, "1 - CEREALS AND CEREAL PRODUCTS.RData")
stocks_file <- file.path(data_dir,"cereals_stocks.RData")
trade_dat <-
    lapply(years, get_trade_data, prod_trade_file, stocks_file,
           mov_avg = 2)
###trade_dat <- get_trade_data(2011, prod_trade_file, stocks_file)

zero_diag <- function(x) {
    diag(x$E0) <- 0
    x
}

trade_dat <- lapply(trade_dat, zero_diag)

names(trade_dat) <- years

# Save in matlab format
writeMat(file.path(res_dir,"CountryNames.mat"), CountryNames = names(trade_dat$`2000`$P0))

# Calculate summaries of trade data: lapply(X, FUN)
trade_st <- bind_rows(lapply(trade_dat, get_trade_stats_sum))

## Load and save production & trade data in matlab format
load(prod_trade_file)
writeMat(file.path(res_dir,"Production_kcal_1986_2011.mat"), Production = Pkbyc)
writeMat(file.path(res_dir,"ExportMatrix_kcal_1986_2011.mat"), Trade = Tkbyc)
writeMat(file.path(res_dir,paste0("dustbowl_disrupt_results_E0_", names(trade_dat)[1], ".mat")), dustbowlResults_E0 = trade_dat$`2000`$E0)
writeMat(file.path(res_dir,paste0("dustbowl_disrupt_results_R0_", names(trade_dat)[1], ".mat")), dustbowlResults_R0 = trade_dat$`2000`$R0)
writeMat(file.path(res_dir,paste0("dustbowl_disrupt_results_P0_", names(trade_dat)[1], ".mat")), dustbowlResults_P0 = trade_dat$`2000`$P0)

writeMat(file.path(res_dir,paste0("dustbowl_disrupt_results_E0_", names(trade_dat)[2], ".mat")), dustbowlResults_E0 = trade_dat$`2009`$E0)
writeMat(file.path(res_dir,paste0("dustbowl_disrupt_results_R0_", names(trade_dat)[2], ".mat")), dustbowlResults_R0 = trade_dat$`2009`$R0)
writeMat(file.path(res_dir,paste0("dustbowl_disrupt_results_P0_", names(trade_dat)[2], ".mat")), dustbowlResults_P0 = trade_dat$`2009`$P0)

# Run sims with range of fr, Fraction of actual reserves 
# that are available to absorb shocks, for multiple time slices

fracR0 = 0.2
fracRvec <- seq(fracR0, 1, 0.2)

for (i in 1:length(fracRvec)) {
    #
    fracR <- fracRvec[i]
    # Inputs for sim_1c function(c_init, a_init, P0, R0, E0, cfrac = 0, asym = TRUE,
    #                            kmax = 50, amin = 1E-5, anim_out = FALSE) #x$coefR *
    result_multiRcoef <-
                   lapply(trade_dat, function(x) sim_1c("USA", 0.4, x$P0, fracR*x$R0, x$E0, cfrac = 0.01, asym = TRUE, kmax = 500, amin = 1E-5, anim_out = FALSE))
    saveRDS(result_multiRcoef, file.path(res_dir, "result_multiRcoef.RData"))

    # Save in matlab format
    writeMat(file.path(res_dir,paste0("dustbowl_disrupt_results_dE_Rcoef", fracR, "_", names(trade_dat)[1], ".mat")), dustbowlResults_dE2000 = result_multiRcoef$`2000`$dE)
    writeMat(file.path(res_dir,paste0("dustbowl_disrupt_results_dR_Rcoef", fracR, "_", names(trade_dat)[1], ".mat")), dustbowlResults_dR2000 = result_multiRcoef$`2000`$dR)
    writeMat(file.path(res_dir,paste0("dustbowl_disrupt_results_dC_Rcoef", fracR, "_", names(trade_dat)[1], ".mat")), dustbowlResults_dC2000 = result_multiRcoef$`2000`$dC)
    
    writeMat(file.path(res_dir,paste0("dustbowl_disrupt_results_dE_Rcoef", fracR, "_", names(trade_dat)[2], ".mat")), dustbowlResults_dE2009 = result_multiRcoef$`2009`$dE)
    writeMat(file.path(res_dir,paste0("dustbowl_disrupt_results_dR_Rcoef", fracR, "_", names(trade_dat)[2], ".mat")), dustbowlResults_dR2009 = result_multiRcoef$`2009`$dR)
    writeMat(file.path(res_dir,paste0("dustbowl_disrupt_results_dC_Rcoef", fracR, "_", names(trade_dat)[2], ".mat")), dustbowlResults_dC2009 = result_multiRcoef$`2009`$dC)
}

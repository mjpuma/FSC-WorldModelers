## Example code to run simulations with the Trade and Food Security cascade model
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

setwd("~/Dropbox/MURI/NuclearWinter")

# Load functions
source("tfs_cascade_funcs.R")
source("res_00_funcs.R")

# Load cereals network data for 2011
data_dir <- "/data"

prod_trade_file <- file.path(data_dir, "1 - CEREALS AND CEREAL PRODUCTS.RData")
stocks_file <- file.path(data_dir,"cereals_stocks.RData")

trade_dat <- get_trade_data(2006, prod_trade_file, stocks_file)
P0 <- trade_dat$P0
R0 <- trade_dat$R0
E0 <- trade_dat$E0

## Save in matlab format
#load(prod_trade_file)
#library(R.matlab)
#writeMat("Production_kcal_1986_2011.mat", Production = Pkbyc)
#writeMat("ExportMatrix_kcal_1986_2011.mat", Trade = Tkbyc)
#rm(trade_dat)

## Run a  simulation originating at multiple countries, with animation
# One vector of country names, one vector of shock intensities, and R does 
# the element-wise multiplication of the intensities with the original data from P0
# Initalize production shock vector

# Run a simulation for multiple disruption scenarions
scenarios <-
    list(
        c(ARG = 0.1, BRA = 0.1, USA = 0.5, RUS = 0.5),
        c(ARG = 0.5, BRA = 0.5, USA = 0.1, RUS = 0.1))

sim_result_mc <- sim_mc_multi(scenarios, P0, R0, E0, cfrac = 0,
                              asym = TRUE, kmax = 50, amin = 1E-5)
# Get a list of summary statistics for sim_result_mc
sim_stats <- get_stats_mcmulti(sim_result_mc) 


# Identify directly affected countries
a_init = setNames(c(0.1, 0.1, 0.5, 0.5),c("ARE", "MUS", "DZA", "CYP"))

# Create animation for multicountry disruption
# Use this for animation
#anim_res <- sim_mc(a_init, P0, R0, E0, cfrac = 0, asym = TRUE,
#                   kmax = 50, amin = 1E-5, anim_out = TRUE)
#saveRDS(anim_res, "sim_anim_ENSO.RData") # Save results for map_test.R

# Use this to check results are correct
simoutput_mc <- sim_mc(a_init, P0, R0, E0, cfrac = 0, asym = TRUE,
                       kmax = 50, amin = 1E-5, anim_out = FALSE)
sim_diagnostics(simoutput_mc) # Tests whether output respects equation: S = P + I - E = R + C

## Save in matlab format
library(R.matlab)
writeMat("CountryNames.mat", CountryNames = names(P0))
writeMat("multi_TradeData2006.mat", multi_TradeData2006 = trade_dat)
writeMat("multi_disrupt_results_E0.mat", multi_Results_E0 = sim_result_mc$E0)
writeMat("multi_disrupt_results_R0.mat", multi_Results_R0 = sim_result_mc$R0)
writeMat("multi_disrupt_results_C0.mat", multi_Results_C0 = sim_result_mc$C0)
writeMat("multi_disrupt_results_dE.mat", multi_Results_dE = sim_result_mc$dE)
writeMat("multi_disrupt_results_dR.mat", multi_Results_dR = sim_result_mc$dR)
writeMat("multi_disrupt_results_dC.mat", multi_Results_dC = sim_result_mc$dC)
writeMat("multi_disrupt_stats.mat", multi_stats = sim_stats)


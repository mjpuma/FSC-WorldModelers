## Main script for the Food Shock Cascade (FSC) Model

# Load FSC functions
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
source("main/FSC_component_funcs.R")

# Step 0: Setup ----
# Default 1 year run for static model
years <- 1 #Don't Change
# Create column names for output files
column_names = c('0', '1')
column_names2 = c('1')

# Create output directory if needed
if (dir.exists("outputs") == FALSE) {
  dir.create("outputs")
}

# Step 1: Specify scenario ----
#  Command line : e.g., "rscript main/main.R 0 1 0.5"
# Parse arguments ====
args <- commandArgs(trailingOnly = TRUE)
i_scenario <- c(as.numeric(args[1]))             # Scenario from the FSC Scenario Library
# #  RStudio or similar integrated development environment (IDE)
# #  Specify arguments  ====
# # Specify working directory
# i_scenario <- 34 # See list below 
# setwd("~/GitHub_mjpuma/FSC-WorldModelers/")


# Step 2: Scenario library (read in files) ----
# Production *fractional declines* list by year by country ====
# Read production declines and export restrictions
# NOTE: Currently export restrictions are coded in Step 7 
#       and correspond to the noted files
#  Index based on iso3 alphabetical ordering
#  Scenario name and production decline fractions
if (i_scenario == 1) {
  name_crop <-c('Wheat')
  runname <- c('Wheat_Avg20152017')
  nameinput <- c('Wheat_Avg20152017')
  shock_scenario <- read.csv(paste0("inputs/Scenario1_COVID_WheatDeclineFraction_1Year_195countries.csv"))
  
} else if (i_scenario == 2) {
  name_crop <-c('Maize')
  runname <- c('Maize_Avg20152017')
  nameinput <- c('Maize_Avg20152017')
  shock_scenario <- read.csv(paste0("inputs/Scenario2_COVID_MaizeDeclineFraction_1Year_195countries.csv"))
  
} else if (i_scenario == 3) {
  name_crop <-c('Rice')
  runname <- c('Rice_Avg20152017')
  nameinput <- c('Rice_Avg20152017')
  shock_scenario <- read.csv(paste0("inputs/Scenario3_COVID_RiceDeclineFraction_1Year_195countries.csv"))

# Wheat scenarios (2021) for breadbasket failure and locust declines 
} else if (i_scenario == 11) {
  name_crop <-c('Wheat')
  runname <- c('Wheat_breadbasket_Year2008')
  nameinput <- c('Wheat_Avg20152017')
  shock_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Wheat_DeclineFraction_breadbasket_Year2008_195countries.csv"))
  #export_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))

} else if (i_scenario == 12) {
  name_crop <-c('Wheat')
  runname <- c('Wheat_locust_Year2020')
  nameinput <- c('Wheat_Avg20152017')
  shock_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Wheat_DeclineFraction_locust_Year2020_195countries.csv"))
  #export_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))
  
} else if (i_scenario == 13) {
  name_crop <-c('Wheat')
  runname <- c('Wheat_locust_Year2020_breadbasket_Year2008')
  nameinput <- c('Wheat_Avg20152017')
  shock_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Wheat_DeclineFraction_locust_Year2020_breadbasket_year2008_195countries.csv"))
  #export_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))

} else if (i_scenario == 14) {
  name_crop <-c('Wheat')
  runname <- c('Wheat_ExportRestrictionFraction_Year2008')
  nameinput <- c('Wheat_Avg20152017')
  shock_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))
  #export_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Wheat_ExportRestrictionFraction_Year2008_195countries.csv"))

} else if (i_scenario == 15) {
  name_crop <-c('Wheat')
  runname <- c('Wheat_ExportRestrictionFraction_Year2008with_RUS_25')
  nameinput <- c('Wheat_Avg20152017')
  shock_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))
  #export_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Wheat_ExportRestrictionFraction_Year2008_with_RUS_25%_195countries.csv"))

} else if (i_scenario == 16) {
  name_crop <-c('Wheat')
  runname <- c('Wheat_ExportRestrictionFraction_Year2008with_RUS_50')
  nameinput <- c('Wheat_Avg20152017')
  shock_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))
  #export_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Wheat_ExportRestrictionFraction_Year2008_with_RUS_50%_195countries.csv"))

# Maize scenarios (2021) for breadbasket failure and locust declines 
} else if (i_scenario == 21) {
  name_crop <-c('Maize')
  runname <- c('Maize_breadbasket_Year2008')
  nameinput <- c('Maize_Avg20152017')
  shock_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Maize_DeclineFraction_breadbasket_Year2008_195countries.csv"))
  #export_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))

} else if (i_scenario == 22) {
  name_crop <-c('Maize')
  runname <- c('Maize_locust_Year2020')
  nameinput <- c('Maize_Avg20152017')
  shock_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Maize_DeclineFraction_locust_Year2020_195countries.csv"))
  #export_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))
  
} else if (i_scenario == 23) {
  name_crop <-c('Maize')
  runname <- c('Maize_locust_Year2020_breadbasket_Year2008')
  nameinput <- c('Maize_Avg20152017')
  shock_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Maize_DeclineFraction_locust_Year2020_breadbasket_year2008_195countries.csv"))
  #export_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))

} else if (i_scenario == 24) {
  name_crop <-c('Maize')
  runname <- c('Maize_ExportRestrictionFraction_Year2008')
  nameinput <- c('Maize_Avg20152017')
  shock_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))
  #export_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Maize_ExportRestrictionFraction_Year2008_195countries.csv"))

} else if (i_scenario == 25) {
  name_crop <-c('Maize')
  runname <- c('Maize_ExportRestrictionFraction_Year2008with_ARG_25')
  nameinput <- c('Maize_Avg20152017')
  shock_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))
  #export_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Maize_ExportRestrictionFraction_Year2008_with_ARG_25%_195countries.csv"))

} else if (i_scenario == 26) {
  name_crop <-c('Maize')
  runname <- c('Maize_ExportRestrictionFraction_Year2008with_ARG_50')
  nameinput <- c('Maize_Avg20152017')
  shock_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))
  #export_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Maize_ExportRestrictionFraction_Year2008_with_ARG_50%_195countries.csv"))

# Rice scenarios (2021) for breadbasket failure and locust declines 
} else if (i_scenario == 31) {
  name_crop <-c('Rice')
  runname <- c('Rice_breadbasket_Year2008')
  nameinput <- c('Rice_Avg20152017')
  shock_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Rice_DeclineFraction_breadbasket_Year2008_195countries.csv"))
  #export_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))

} else if (i_scenario == 32) {
  name_crop <-c('Rice')
  runname <- c('Rice_locust_Year2020')
  nameinput <- c('Rice_Avg20152017')
  shock_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Rice_DeclineFraction_locust_Year2020_195countries.csv"))
  #export_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))
  
} else if (i_scenario == 33) {
  name_crop <-c('Rice')
  runname <- c('Rice_locust_Year2020_breadbasket_Year2008')
  nameinput <- c('Rice_Avg20152017')
  shock_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Rice_DeclineFraction_locust_Year2020_breadbasket_year2008_195countries.csv"))
  #export_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))

} else if (i_scenario == 34) {
  name_crop <-c('Rice')
  runname <- c('Rice_ExportRestrictionFraction_Year2008')
  nameinput <- c('Rice_Avg20152017')
  shock_scenario<- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_NoChange_195countries.csv"))
  #export_scenario <- read.csv(paste0("inputs/2021_C2P2_scenarios/Scenario_Rice_ExportRestrictionFraction_Year2008_195countries.csv"))

}


# Step 3: Load ancillary data ----
# i) Commodity list for bilateral trade
commodities <- read.csv(paste0("ancillary/", nameinput, "cropcommodity_tradelist.csv"))
# ii) Load country list
country_list <- read.csv("ancillary/country_list195_2012to2016.csv")
country_list <- country_list[order(country_list$iso3), ] # Order by iso3 code


# Step 4: Load production/trade/stocks data ----
load(paste0("inputs_processed/", nameinput, "E0.RData")) #Export Matrix ordered by FAOSTAT country code (increasing)
load(paste0("inputs_processed/", nameinput, "P0.Rdata")) #Production
load(paste0("inputs_processed/", nameinput, "R0.RData")) #Reserves (a.k.a. Stocks)


# Step 5: Setup production and shocks; initialize output vectors ----
iso_order <- as.character(country_list$iso3)
if (!setequal(as.character(Pkbyc$iso3), iso_order)) {
  stop("Production iso3 codes do not match the country list")
}
Pkbyc <- Pkbyc[match(iso_order, as.character(Pkbyc$iso3)), ]
P0 <- Pkbyc

# Create 'Shocks' dataframe ====
Shocks <- merge(country_list,shock_scenario,by = 'iso3')
Shocks[is.na(Shocks)] <- 0

# Order shocks dataframe by FAOSTAT country code (in increasing order) ====
P <- P0
Prod <- as.numeric(unlist(P0$P0))
Shocks <- merge(Shocks, P, by = "iso3")
Shocks <- Shocks[order(Shocks$iso3), ]
if (!("year_1" %in% names(Shocks))) {
  stop("Static scenarios require a year_1 shock column")
}
Shocks$year_1 <- clamp_shock_fraction(Shocks$year_1, Shocks$iso3, "year_1")

# # Create Export Restriction dataframe ====
# ExportRestrict <- merge(country_list,export_scenario,by = 'iso3')
# ExportRestrict[is.na(ExportRestrict)] <- 0

# # Order ExportRestrict dataframe by FAOSTAT country code (in increasing order) ====
# ExportRestrict <- merge(ExportRestrict, P, by = "iso3")
# ExportRestrict <- ExportRestrict[order(ExportRestrict$iso3), ]

# Initialize  output vectors ====
Pout <-  array(0, c(nrow(country_list), length(years) + 1))
Rout <-  array(0, c(nrow(country_list), length(years) + 1))
Cout <-  array(0, c(nrow(country_list), length(years) + 1))
Eout <-  array(0, c(nrow(country_list), nrow(country_list), length(years) + 1))

shortageout <- array(0, c(nrow(country_list), length(years)))
C1_C0out <- array(0, c(nrow(country_list), length(years)))
C2_C0out <- array(0, c(nrow(country_list), length(years)))
dR_C0out <- array(0, c(nrow(country_list), length(years)))

## Add initial conditions to output arrays
E0 <- E0_avg
if (!identical(rownames(E0), iso_order) || !identical(colnames(E0), iso_order)) {
  stop("Export matrix dimnames are not the iso3-sorted country list")
}
assert_no_self_loops(E0)
if (!setequal(names(R0), iso_order)) {
  stop("Reserve names do not match the country list")
}
R0 <- R0[iso_order]
names(Prod) <- iso_order
Eout[, , 1] <- E0
Pout[, 1] <- Prod
Rout[, 1] <- R0

## Create 'InputFSC' dataframe adding initial reserves====
InputFSC <- data_frame(iso3 = names(R0), R0 = R0)

# Step 6: Apply the clamped production shock ----
# Positive year_1 values are declines. Negative values are gains, added to reserves.
shock_frac <- Shocks$year_1
FracGain <- pmax(-shock_frac, 0)
FracLoss <- pmax(shock_frac, 0)
dP <- -FracLoss * Shocks$P0
names(dP) <- Shocks$iso3
Shocks$dP <- dP

InputFSC <- merge(Shocks, InputFSC, by = "iso3", all.x = TRUE, all.y = FALSE)
InputFSC <- InputFSC[order(InputFSC$iso3), ]
InputFSC$Rcurrent <- pmax(0, InputFSC$R0 + InputFSC$P0 * FracGain)
Rcurrent <- as.numeric(InputFSC$Rcurrent)
names(Rcurrent) <- InputFSC$iso3

trade_dat <- list(P = Prod, R = Rcurrent, E = E0)
names(trade_dat$P) <- iso_order
names(trade_dat$R) <- iso_order
trade_dat$nc <- length(trade_dat$P)
trade_dat$dR <- rep(0, trade_dat$nc)
trade_dat$C <- trade_dat$P + colSums(trade_dat$E) - rowSums(trade_dat$E)
C0_initial <- trade_dat$C
Cout[, 1] <- C0_initial
trade_dat$shortage <- rep(0, trade_dat$nc)

# Supply after the production change and before export bans.
trade_dat$C1 <- trade_dat$P + Shocks$dP + colSums(trade_dat$E) - rowSums(trade_dat$E)
ImportsInitial <- colSums(trade_dat$E)
ExportsInitial <- rowSums(trade_dat$E)

# Step 7: Export bans ----
# fraction = share of the export row that is removed. That volume is added to
# reserves once, and the row is multiplied by (1 - fraction).
# Scenarios 2 and 3 use the crop that the scenario library assigns:
#   2 = maize (ARG, BRA, UKR), 3 = rice (IND, THA, VNM).
ban_fraction <- rep(0, nrow(E0))
names(ban_fraction) <- rownames(E0)
covid_bans <- list(
  "1" = c("RUS", "UKR", "KAZ"),
  "2" = c("ARG", "BRA", "UKR"),
  "3" = c("IND", "THA", "VNM")
)
export_files <- c(
  "14" = "inputs/2021_C2P2_scenarios/Scenario_Wheat_ExportRestrictionFraction_Year2008_195countries.csv",
  "15" = "inputs/2021_C2P2_scenarios/Scenario_Wheat_ExportRestrictionFraction_Year2008_with_RUS_25%_195countries.csv",
  "16" = "inputs/2021_C2P2_scenarios/Scenario_Wheat_ExportRestrictionFraction_Year2008_with_RUS_50%_195countries.csv",
  "24" = "inputs/2021_C2P2_scenarios/Scenario_Maize_ExportRestrictionFraction_Year2008_195countries.csv",
  "25" = "inputs/2021_C2P2_scenarios/Scenario_Maize_ExportRestrictionFraction_Year2008_with_ARG_25%_195countries.csv",
  "26" = "inputs/2021_C2P2_scenarios/Scenario_Maize_ExportRestrictionFraction_Year2008_with_ARG_50%_195countries.csv",
  "34" = "inputs/2021_C2P2_scenarios/Scenario_Rice_ExportRestrictionFraction_Year2008_195countries.csv"
)
scenario_key <- as.character(i_scenario)
if (scenario_key %in% names(covid_bans)) {
  missing_iso <- setdiff(covid_bans[[scenario_key]], names(ban_fraction))
  if (length(missing_iso)) {
    stop("Export-ban iso3 missing from the network: ", paste(missing_iso, collapse = ", "))
  }
  ban_fraction[covid_bans[[scenario_key]]] <- 1
} else if (scenario_key %in% names(export_files)) {
  export_scenario <- read.csv(export_files[[scenario_key]])
  if (!all(c("iso3", "year_1") %in% names(export_scenario))) {
    stop("Export-restriction file must contain iso3 and year_1")
  }
  export_scenario$year_1 <- clamp_shock_fraction(
    export_scenario$year_1, export_scenario$iso3, "export restriction"
  )
  # Declines in this file are ban shares in [0, 1]. Gains are not meaningful here.
  export_scenario$year_1 <- pmax(export_scenario$year_1, 0)
  ban_fraction[export_scenario$iso3] <- export_scenario$year_1
  ban_fraction[is.na(ban_fraction)] <- 0
}

banned <- apply_export_bans(E0, Rcurrent, ban_fraction)
E0 <- banned$E
Rcurrent <- banned$reserves
if (any(banned$removed > 0)) {
  message("Export bans removed volume from ", sum(banned$removed > 0), " countries")
}

# Step 8: Update food balance variables ----
trade_dat$R <- Rcurrent
trade_dat$E <- E0
trade_dat$C2 <- trade_dat$P + Shocks$dP + colSums(trade_dat$E) - rowSums(trade_dat$E)
# Domestic loss relative to positive baseline consumption. Baseline re-export
# deficits (negative C0) are not counted as a new shortage.
trade_dat$shortage <- pmax(0, pmax(C0_initial, 0) - pmax(trade_dat$C2, 0))

ImportsFinal <- colSums(trade_dat$E)
ExportsFinal <- rowSums(trade_dat$E)

results_FSCstatic <- list(
  P = pmax(0, trade_dat$P + Shocks$dP),
  R = pmax(0, Rcurrent),
  C1 = trade_dat$C1,
  C2 = trade_dat$C2,
  shortage = trade_dat$shortage,
  E = E0
)

i <- 1
Pout[, i + 1] <- as.numeric(results_FSCstatic$P)
Rout[, i + 1] <- as.numeric(results_FSCstatic$R)
Cout[, i + 1] <- as.numeric(results_FSCstatic$C1)
shortageout[, i] <- as.numeric(results_FSCstatic$shortage)
C1_C0out[, i] <- safe_ratio(as.numeric(results_FSCstatic$C1), C0_initial)
C2_C0out[, i] <- safe_ratio(as.numeric(results_FSCstatic$C2), C0_initial)

# Step 9: Collect, reformat and save output data ----
# Production
colnames(Pout)  <- column_names
rownames(Pout)  <- InputFSC$iso3
Pout_df <- data.frame(Pout)
Pout_df <- tibble::rownames_to_column(Pout_df, "iso3")
Pout_df <- merge(InputFSC[, c("iso3", "Country.x")], Pout_df, by = "iso3")
# combine the year columns into a single column with separate rows for each year; assign to new vector
Pout_df <- gather(Pout_df, Year, Value, -iso3, -Country.x)
# remove preceeding X character for Year column aand convert to numeric
Pout_df$Year <- as.numeric(gsub("[a-zA-Z ]", "", Pout_df$Year))

# Reserves
colnames(Rout)  <- column_names
rownames(Rout)  <- InputFSC$iso3
Rout_df <- data.frame(Rout)
Rout_df <- tibble::rownames_to_column(Rout_df, "iso3")
Rout_df <- merge(InputFSC[, c("iso3", "Country.x")], Rout_df, by = "iso3")
# combine the year columns into a single column with separate rows for each year; assign to new vector
Rout_df <- gather(Rout_df, Year, Value, -iso3, -Country.x)
# remove preceeding X character for Year column aand convert to numeric
Rout_df$Year <- as.numeric(gsub("[a-zA-Z ]", "", Rout_df$Year))

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
  merge(InputFSC[, c("iso3", "Country.x")], shortageout_df, by = "iso3")
# combine the year columns into a single column with separate rows for each year; assign to new vector
shortageout_df <- gather(shortageout_df, Year, Value, -iso3, -Country.x)
# remove preceeding X character for Year column aand convert to numeric
shortageout_df$Year <-
  as.numeric(gsub("[a-zA-Z ]", "", shortageout_df$Year))

# Consumption to C0
colnames(C1_C0out)  <- column_names2
rownames(C1_C0out)  <- InputFSC$iso3
C1_C0out_df <- data.frame(C1_C0out)
C1_C0out_df <- tibble::rownames_to_column(C1_C0out_df, "iso3")
C1_C0out_df <-
  merge(InputFSC[, c("iso3", "Country.x")], C1_C0out_df, by = "iso3")
# combine the year columns into a single column with separate rows for each year; assign to new vector
C1_C0out_df <- gather(C1_C0out_df, Year, Value, -iso3, -Country.x)
# remove preceeding X character for Year column aand convert to numeric
C1_C0out_df$Year <- as.numeric(gsub("[a-zA-Z ]", "", C1_C0out_df$Year))

colnames(C2_C0out)  <- column_names2
rownames(C2_C0out)  <- InputFSC$iso3
C2_C0out_df <- data.frame(C2_C0out)
C2_C0out_df <- tibble::rownames_to_column(C2_C0out_df, "iso3")
C2_C0out_df <-
  merge(InputFSC[, c("iso3", "Country.x")], C2_C0out_df, by = "iso3")
# combine the year columns into a single column with separate rows for each year; assign to new vector
C2_C0out_df <- gather(C2_C0out_df, Year, Value, -iso3, -Country.x)
# remove preceeding X character for Year column and convert to numeric
C2_C0out_df$Year <- as.numeric(gsub("[a-zA-Z ]", "", C2_C0out_df$Year))

## Save as CSV
write.csv(Pout_df, paste0("outputs/",runname,"ProductionStatic.csv"), row.names = FALSE)
write.csv(Rout_df, paste0("outputs/",runname,"ReserveStatic.csv"), row.names = FALSE)
write.csv(shortageout_df, paste0("outputs/",runname,"ShortageStatic.csv"), row.names = FALSE)
write.csv(C1_C0out_df, paste0("outputs/",runname,"C1C0Static.csv"), row.names = FALSE)
write.csv(C2_C0out_df, paste0("outputs/",runname,"C2C0Static.csv"), row.names = FALSE)
write.csv(Eout, paste0("outputs/",runname, "ExportStatic.csv"), row.names = TRUE)

## Save lists of initial and final imports and outputs by country
write.csv(ExportsFinal, paste0("outputs/",runname, "ExportsFinal.csv"), row.names = FALSE)
write.csv(ExportsInitial, paste0("outputs/",runname, "ExportsInitial.csv"), row.names = FALSE)
write.csv(ImportsFinal, paste0("outputs/",runname, "ImportsFinal.csv"), row.names = FALSE)
write.csv(ImportsInitial, paste0("outputs/",runname, "ImportsInitial.csv"), row.names = FALSE)

# Save Exports as R data file
saveRDS(Eout, file = paste0("outputs/", runname,"ExportStatic.rds"))

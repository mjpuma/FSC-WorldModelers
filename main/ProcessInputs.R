## Build trade, production, and reserve baselines for the Food Shock Cascade model.
##
## Run from the repository root:
##   Rscript main/ProcessInputs.R --crop wheat --years 2015:2017 --unit kcal --prefix Wheat_rebuild_check
##   Rscript main/ProcessInputs.R --crop rice --years 2018:2021 --unit tonnes --milling-rate 0.67
##
## The FAO detailed trade matrix is required and is not shipped in this checkout.
## Existing Avg20152017 baselines are left in place unless --overwrite is set.
## Every run writes a stock-status report. Missing PSD rows stay in that report
## and are not treated as silent, untracked zeros.

source("main/ProcessInputs_funcs.R")

args <- commandArgs(trailingOnly = TRUE)
usage <- paste(
    "Usage: Rscript main/ProcessInputs.R --crop wheat|maize|rice|soybean",
    "       --years 2018:2021 --unit kcal|tonnes [--prefix NAME]",
    "       [--milling-rate 0.67] [--overwrite] [--allow-missing-stocks]",
    "",
    "Reads inputs/Production_Crops_Livestock_E_All_Data_(Normalized).csv (or the older",
    "Production_Crops_E file), the FAO detailed trade matrix, and inputs/psd_grains_pulses.csv.",
    "Soybean stocks are not in the grains PSD file. Pass --allow-missing-stocks only if a",
    "zero stock vector, with every country flagged, is acceptable.",
    sep = "\n"
)
if (!length(args) || any(args %in% c("-h", "--help"))) {
    cat(usage, "\n")
    quit(save = "no", status = 0)
}

opt <- list(
    crop = "wheat",
    years = "2015:2017",
    unit = "kcal",
    prefix = NA_character_,
    milling_rate = 0.67,
    overwrite = FALSE,
    allow_missing_stocks = FALSE
)
i <- 1
while (i <= length(args)) {
    key <- args[[i]]
    if (key %in% c("--overwrite", "--allow-missing-stocks")) {
        opt[[sub("^--", "", key)]] <- TRUE
        if (key == "--allow-missing-stocks") opt$allow_missing_stocks <- TRUE
        i <- i + 1
        next
    }
    if (i == length(args)) stop("Missing value after ", key)
    val <- args[[i + 1]]
    if (key == "--crop") opt$crop <- tolower(val)
    else if (key == "--years") opt$years <- val
    else if (key == "--unit") opt$unit <- tolower(val)
    else if (key == "--prefix") opt$prefix <- val
    else if (key == "--milling-rate") opt$milling_rate <- as.numeric(val)
    else stop("Unknown argument: ", key, "\n", usage)
    i <- i + 2
}

root <- fsc_root()
setwd(root)
years <- parse_year_arg(opt$years)
if (!is.finite(opt$milling_rate) || opt$milling_rate <= 0 || opt$milling_rate > 1) {
    stop("--milling-rate must be in (0, 1].")
}
spec <- crop_spec_from_lists(opt$crop, opt$unit, milling_rate = opt$milling_rate)
stem <- switch(opt$crop, wheat = "Wheat", maize = "Maize", rice = "Rice", soybean = "Soybean")
if (is.na(opt$prefix)) {
    opt$prefix <- if (length(years) == 1) {
        sprintf("%s_Y%s", stem, years)
    } else {
        sprintf("%s_Avg%s%s", stem, min(years), max(years))
    }
}

out_dir <- "inputs_processed"
targets <- c(
    file.path(out_dir, paste0(opt$prefix, "E0.RData")),
    file.path(out_dir, paste0(opt$prefix, "P0.Rdata")),
    file.path(out_dir, paste0(opt$prefix, "R0.RData"))
)
if (any(file.exists(targets)) && !opt$overwrite) {
    stop(
        "Refusing to overwrite existing baseline files:\n",
        paste(targets[file.exists(targets)], collapse = "\n"),
        "\nPass --overwrite to replace them, or choose another --prefix."
    )
}

prod_candidates <- c(
    "inputs/Production_Crops_Livestock_E_All_Data_(Normalized).csv",
    "inputs/Production_Crops_E_All_Data_(Normalized).csv"
)
trade_candidates <- c(
    "inputs/Trade_DetailedTradeMatrix_E_All_Data_(Normalized).csv",
    "inputs/Trade_DetailedTradeMatrix_E_All_Data_Normalized.csv"
)
prod_path <- prod_candidates[file.exists(prod_candidates)][1]
trade_path <- trade_candidates[file.exists(trade_candidates)][1]
psd_path <- "inputs/psd_grains_pulses.csv"
if (is.na(prod_path)) stop("FAO production file not found in inputs/.")
if (is.na(trade_path)) {
    stop(
        "FAO detailed trade matrix was not found. Download ",
        "Trade_DetailedTradeMatrix_E_All_Data_(Normalized).csv into inputs/ ",
        "before building a baseline. The 2015-2017 networks already in ",
        "inputs_processed/ do not require this step."
    )
}
if (!file.exists(psd_path)) stop("USDA PSD file not found: ", psd_path)

message("Reading production from ", prod_path)
prod_raw <- read_fsc_table(prod_path)
message("Reading trade from ", trade_path)
trade_raw <- read_fsc_table(trade_path)
message("Reading stocks from ", psd_path)
psd_raw <- read_fsc_table(psd_path)

prod <- normalize_production_table(prod_raw)
trade <- normalize_trade_table(trade_raw)
stocks <- normalize_psd_table(psd_raw)
rm(prod_raw, trade_raw, psd_raw)

# Successor years are required only when the requested window still contains a
# predecessor aggregate (Sudan through 2011, Serbia and Montenegro through 2005).
extra_years <- integer()
if (any(years <= 2011)) extra_years <- c(extra_years, 2012:2014)
if (any(years <= 2005)) extra_years <- c(extra_years, 2006:2008)
keep_prod_years <- unique(c(years, extra_years))
item_ids <- unique(c(spec$prod_ids, spec$trade_ids))
prod <- prod[prod$Year %in% keep_prod_years & prod$itemCode %in% item_ids, , drop = FALSE]
trade <- trade[trade$Year %in% years & trade$itemCode %in% spec$trade_ids, , drop = FALSE]

if (opt$crop == "soybean" && !any(stocks$Product == "Soybeans") && !opt$allow_missing_stocks) {
    stop(
        "inputs/psd_grains_pulses.csv has no Soybeans ending stocks. ",
        "Supply an oilseed PSD extract or pass --allow-missing-stocks. ",
        "That flag writes zeros and marks every country as missing_psd_row."
    )
}

country_list <- read.csv("ancillary/country_list195_2012to2016.csv", stringsAsFactors = FALSE)
built <- build_fsc_baseline(
    prod = prod, trade = trade, stocks = stocks, country_list = country_list,
    years = years, unit = opt$unit, spec = spec
)

if (!dir.exists(out_dir)) dir.create(out_dir)
E0_avg <- built$E0_avg
Pkbyc <- built$Pkbyc
R0 <- built$R0
save(E0_avg, file = targets[[1]])
save(Pkbyc, file = targets[[2]])
save(R0, file = targets[[3]])
write.csv(Pkbyc, file.path(out_dir, paste0(opt$prefix, "Production.csv")), row.names = FALSE)
write.csv(data.frame(iso3 = names(R0), R0 = as.numeric(R0)),
          file.path(out_dir, paste0(opt$prefix, "Reserves.csv")), row.names = FALSE)
write.csv(E0_avg, file.path(out_dir, paste0(opt$prefix, "E0.csv")))
write.csv(built$stocks_report, file.path(out_dir, paste0(opt$prefix, "_stocks_report.csv")),
          row.names = FALSE)
write.csv(built$reexport_report, file.path(out_dir, paste0(opt$prefix, "_reexport_report.csv")),
          row.names = FALSE)
writeLines(built$log, file.path(out_dir, paste0(opt$prefix, "_build_log.txt")))
message("Wrote ", opt$prefix, " (", opt$unit, ") for ", min(years), "-", max(years))
message(paste(built$log, collapse = "\n"))

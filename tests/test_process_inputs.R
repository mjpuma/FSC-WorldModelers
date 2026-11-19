## Harmonization checks that do not need the FAO bulk files.
##   Rscript tests/test_process_inputs.R

source("main/ProcessInputs_funcs.R")

failures <- character()
check <- function(cond, label) {
    if (!isTRUE(cond)) {
        failures <<- c(failures, label)
        message("FAIL: ", label)
    } else {
        message("OK: ", label)
    }
}

# Element filter and spaced FAO headers.
raw_prod <- data.frame(
    `Area Code` = c(1, 1, 1),
    `Item Code` = c(27, 27, 56),
    Element = c("Area harvested", "Production", "Production"),
    Year = c(2018, 2018, 2018),
    Value = c(999, 100, 4),
    check.names = FALSE,
    stringsAsFactors = FALSE
)
norm_prod <- normalize_production_table(raw_prod)
check(nrow(norm_prod) == 2 && all(norm_prod$value %in% c(100, 4)),
      "production reader keeps the Production element only")

raw_trade <- data.frame(
    `Reporter Country Code` = c(2, 2),
    `Partner Country Code` = c(1, 2),
    `Item Code` = c(31, 31),
    Element = c("Export Quantity", "Import Quantity"),
    Year = c(2018, 2018),
    Value = c(5, 8),
    check.names = FALSE,
    stringsAsFactors = FALSE
)
norm_trade <- normalize_trade_table(raw_trade)
check(nrow(norm_trade) == 1 && norm_trade$value == 5,
      "trade reader keeps export quantity only")

# Predecessor splits use later successor production as the backward key.
split_prod <- data.frame(
    FAOST_CODE = c(186, 272, 273, 206, 276, 277),
    Year = c(2002, 2006, 2006, 2010, 2012, 2012),
    itemCode = rep(15L, 6),
    value = c(100, 90, 10, 50, 80, 20)
)
split_res <- apply_predecessor_splits(split_prod)
srb <- split_res$prod$value[split_res$prod$FAOST_CODE == 272 & split_res$prod$Year == 2002]
mne <- split_res$prod$value[split_res$prod$FAOST_CODE == 273 & split_res$prod$Year == 2002]
sdn <- split_res$prod$value[split_res$prod$FAOST_CODE == 276 & split_res$prod$Year == 2010]
ssd <- split_res$prod$value[split_res$prod$FAOST_CODE == 277 & split_res$prod$Year == 2010]
check(abs(srb - 90) < 1e-8 && abs(mne - 10) < 1e-8, "Serbia and Montenegro split by later production shares")
check(abs(sdn - 40) < 1e-8 && abs(ssd - 10) < 1e-8, "former Sudan split by 2012 production shares")
check(!any(split_res$prod$FAOST_CODE %in% c(186, 206)), "predecessor codes are removed after the split")

# Rice in milled tonnes: paddy x 0.67, milled trade unchanged, bran oil dropped,
# missing stocks flagged, self-loops removed, re-exports flagged.
country_list <- data.frame(
    FAOST_CODE = c(1L, 2L, 3L),
    iso3 = c("AAA", "BBB", "CCC"),
    Country = c("Alpha", "Beta", "Gamma"),
    stringsAsFactors = FALSE
)
spec <- list(
    prod_ids = c(27L, 31L),
    trade_ids = c(27L, 31L, 36L),
    kcal = c("27" = 1, "31" = 1, "36" = 1),
    tonne_factor = c("27" = 0.67, "31" = 1, "36" = 0),
    reserve_products = "Rice, Milled",
    reserve_kcal = 3600000,
    reserve_tonne_factor = 1,
    excluded_items = "Rice bran oil"
)
prod <- data.frame(FAOST_CODE = 1L, Year = 2018L, itemCode = 27L, value = 100)
trade <- data.frame(
    reporter = c(2L, 2L, 1L),
    partner = c(3L, 1L, 1L),
    Year = c(2018L, 2018L, 2018L),
    itemCode = c(31L, 31L, 31L),
    value = c(20, 5, 9)
)
stocks <- data.frame(
    Country = c("Alpha", "Gamma", "Hong Kong"),
    Year = c(2018L, 2018L, 2018L),
    Product = c("Rice, Milled", "Rice, Milled", "Rice, Milled"),
    value = c(2, 0, 7),
    stringsAsFactors = FALSE
)
# Hong Kong is not in this toy list. The name map must not crash, and the
# unmatched-name log records it. Gamma is an explicit zero. Beta has no row.
built <- build_fsc_baseline(
    prod = prod, trade = trade, stocks = stocks, country_list = country_list,
    years = 2018, unit = "tonnes", spec = spec, splits = list(), eu_members = character()
)
check(abs(built$Pkbyc$P0[built$Pkbyc$iso3 == "AAA"] - 67) < 1e-8,
      "paddy production converts at 0.67 milled tonnes")
check(all(abs(diag(built$E0_avg)) == 0), "built trade matrix has a zero diagonal")
check(abs(built$E0_avg["BBB", "CCC"] - 20) < 1e-8, "milled trade stays in milled tonnes")
check(abs(built$R0["AAA"] - 2000) < 1e-6, "PSD 1000-tonne units become metric tons")
status <- setNames(built$stocks_report$status, built$stocks_report$iso3)
check(status["BBB"] == "missing_psd_row", "a missing PSD country is flagged")
check(status["CCC"] == "psd_true_zero", "an explicit PSD zero is not called missing")
check(status["AAA"] == "psd_reported", "a reported stock is labeled")
check(isTRUE(built$reexport_report$exports_exceed_supply[built$reexport_report$iso3 == "BBB"]),
      "a re-export hub with exports above supply is flagged")
check(any(grepl("Hong Kong", built$log)), "unmatched PSD names are written to the log")
check(any(grepl("self-trade", built$log)), "self-trade rows are logged")

hk <- harmonize_psd_names("Hong Kong")
check(hk == "Hong Kong SAR, China", "Hong Kong PSD name matches the country list")
check(harmonize_psd_names("Eswatini") == "Swaziland", "Eswatini maps to the list name Swaziland")
check(harmonize_psd_names("Taiwan") == "Taiwan", "Taiwan stays a separate node")

rice_spec <- crop_spec_from_lists("rice", "tonnes", milling_rate = 0.67)
check(abs(rice_spec$tonne_factor["27"] - 0.67) < 1e-8, "rice paddy factor is the milling rate")
check(!("36" %in% names(rice_spec$tonne_factor)), "rice bran oil is excluded from tonne mode")
soy_err <- tryCatch(crop_spec_from_lists("soybean", "kcal"), error = function(e) e)
check(inherits(soy_err, "error"), "soybean kilocalorie mode is refused without a stock and factor list")

if (length(failures)) stop(paste(failures, collapse = "\n"))
message("All input-harmonization checks passed.")

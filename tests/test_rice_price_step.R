## Reduced-form rice price index. Run from the repository root:
##   Rscript tests/test_rice_price_step.R

source("R/rice_price_step.R")

failures <- character()
check <- function(cond, label) {
    if (!isTRUE(cond)) {
        failures <<- c(failures, label)
        message("FAIL: ", label)
    } else {
        message("OK: ", label)
    }
}

baseline <- 2018:2021
rows <- list()
for (year in c(baseline, 2022L)) {
    rows[[length(rows) + 1L]] <- data.frame(
        iso3 = "AAA",
        year = year,
        production_mt = if (year == 2022L) 70 else 100,
        domestic_use_mt = 40,
        beginning_stocks_mt = if (year == 2022L) 10 else 20,
        exports_mt = 50,
        reserves_imputed_mt = NA_real_,
        PSD_Missing_Flag = 0L,
        stringsAsFactors = FALSE
    )
    rows[[length(rows) + 1L]] <- data.frame(
        iso3 = "BBB",
        year = year,
        production_mt = 60,
        domestic_use_mt = 20,
        beginning_stocks_mt = 8,
        exports_mt = 10,
        reserves_imputed_mt = NA_real_,
        PSD_Missing_Flag = 0L,
        stringsAsFactors = FALSE
    )
    rows[[length(rows) + 1L]] <- data.frame(
        iso3 = "CCC",
        year = year,
        production_mt = 5,
        domestic_use_mt = 5,
        beginning_stocks_mt = 1,
        exports_mt = 0,
        reserves_imputed_mt = NA_real_,
        PSD_Missing_Flag = 0L,
        stringsAsFactors = FALSE
    )
}
df <- do.call(rbind, rows)

fit <- rice_price_step(df, baseline, beta_surplus = 2, beta_stu = 1, use_imputed = FALSE)
aaa <- fit$exporters[fit$exporters$iso3 == "AAA" & fit$exporters$year == 2022L, ]
bbb <- fit$exporters[fit$exporters$iso3 == "BBB" & fit$exporters$year == 2022L, ]
ccc <- fit$exporters[fit$exporters$iso3 == "CCC" & fit$exporters$year == 2022L, ]
world <- fit$world[fit$world$year == 2022L, ]

expected_aaa <- exp(2 * (30 / 60 - 1) + log(0.25 / 0.5))
expected_world <- (50 * expected_aaa + 10 * 1) / 60

check(abs(aaa$exportable_surplus_mt - 30) < 1e-8, "AAA 2022 surplus is production minus use")
check(abs(aaa$surplus_shock - (-0.5)) < 1e-8, "AAA surplus shock is -0.5")
check(abs(aaa$price_factor - expected_aaa) < 1e-8, "AAA price factor matches the reduced form")
check(isTRUE(aaa$in_index), "AAA is in the 2022 index")
check(abs(bbb$price_factor - 1) < 1e-8, "BBB price factor is 1 when surplus and stocks are unchanged")
check(isTRUE(bbb$in_index), "BBB is in the 2022 index")
check(!isTRUE(ccc$in_index), "CCC zero baseline surplus stays out of the index")
check(abs(world$rice_price_index - expected_world) < 1e-8, "2022 world index is export-weighted")
check(world$n_exporters == 2L, "2022 index uses two exporters")

# A zero stock makes the log term undefined, so the factor is missing.
# The imputed stock restores the baseline stock-to-use.
thin <- df
thin$PSD_Missing_Flag[thin$iso3 == "AAA"] <- 1L
thin$beginning_stocks_mt[thin$iso3 == "AAA"] <- 0
thin$reserves_imputed_mt[thin$iso3 == "AAA" & thin$year == 2022L] <- 10
thin$reserves_imputed_mt[thin$iso3 == "AAA" & thin$year %in% baseline] <- 20
raw <- rice_price_step(thin, baseline, beta_surplus = 2, beta_stu = 1, use_imputed = FALSE)
filled <- rice_price_step(thin, baseline, beta_surplus = 2, beta_stu = 1, use_imputed = TRUE)
raw_aaa <- raw$exporters[raw$exporters$iso3 == "AAA" & raw$exporters$year == 2022L, ]
filled_aaa <- filled$exporters[filled$exporters$iso3 == "AAA" & filled$exporters$year == 2022L, ]
check(is.na(raw_aaa$price_factor), "zero stock leaves the price factor undefined")
check(abs(filled_aaa$price_factor - expected_aaa) < 1e-8, "imputed stocks restore the AAA price factor")

err <- tryCatch(
    rice_price_step(df[, setdiff(names(df), "exports_mt")], baseline, 1, 1),
    error = function(e) e
)
check(inherits(err, "error"), "missing exports_mt is rejected")

if (length(failures)) {
    stop(paste(failures, collapse = "; "))
}
message("All rice price checks passed")

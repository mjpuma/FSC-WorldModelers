## End-to-end smoke test on the processed 2015-2017 network.
## Does not call main.R, so it does not overwrite outputs/*.csv.
##
## From the repository root:
##   Rscript tests/run_fsc_smoke.R
##   Rscript tests/run_fsc_smoke.R maize 0.5
##
## There is no 2018-2021 baseline in this checkout. The run uses
## inputs_processed/<Crop>_Avg20152017 and writes only to outputs/smoke_test/.

library(dplyr, warn.conflicts = FALSE)
source("main/FSC_component_funcs.R")
source("main/FSC_sim_funcs.R")

args <- commandArgs(trailingOnly = TRUE)
crop <- if (length(args) >= 1) tolower(args[[1]]) else "wheat"
alpha <- if (length(args) >= 2) as.numeric(args[[2]]) else 0.5
alpha <- clamp_alpha(alpha)
stem <- switch(crop,
               wheat = "Wheat_Avg20152017",
               maize = "Maize_Avg20152017",
               rice = "Rice_Avg20152017",
               stop("Smoke crop must be wheat, maize, or rice"))

load_baseline <- function(stem_name) {
    e_env <- new.env()
    p_env <- new.env()
    r_env <- new.env()
    load(paste0("inputs_processed/", stem_name, "E0.RData"), envir = e_env)
    load(paste0("inputs_processed/", stem_name, "P0.Rdata"), envir = p_env)
    load(paste0("inputs_processed/", stem_name, "R0.RData"), envir = r_env)
    if (!exists("E0_avg", envir = e_env)) stop(stem_name, " is missing E0_avg")
    list(E = e_env$E0_avg, Pk = p_env$Pkbyc, R0 = r_env$R0)
}

failures <- character()
check <- function(cond, label) {
    if (!isTRUE(cond)) {
        failures <<- c(failures, label)
        message("FAIL: ", label)
    } else {
        message("OK: ", label)
    }
}

# Alignment of every processed crop, including the ones we do not shock.
for (other in c("Wheat_Avg20152017", "Maize_Avg20152017", "Rice_Avg20152017")) {
    base_i <- load_baseline(other)
    iso_i <- as.character(base_i$Pk$iso3)
    check(identical(rownames(base_i$E), iso_i) && identical(colnames(base_i$E), iso_i),
          paste(other, "export matrix is iso3 aligned"))
    check(identical(names(base_i$R0), iso_i), paste(other, "reserves are iso3 aligned"))
    diag_ok <- tryCatch(assert_no_self_loops(base_i$E), error = function(e) e)
    check(!inherits(diag_ok, "error"), paste(other, "trade diagonal is zero"))
}

base <- load_baseline(stem)
iso <- as.character(base$Pk$iso3)
P <- setNames(as.numeric(base$Pk$P0), iso)
R0 <- base$R0[iso]
E <- base$E
domestic0 <- P + colSums(E) - rowSums(E)
n_neg <- sum(domestic0 < -1)
message(sprintf("%s baseline countries with exports above supply: %d", stem, n_neg))

make_net <- function(reserves) {
    list(
        P = P,
        R = reserves,
        E = E,
        dR = setNames(rep(0, length(iso)), iso),
        C = P + colSums(E) - rowSums(E),
        nc = length(iso),
        shortage = setNames(rep(0, length(iso)), iso)
    )
}

shock_of <- function(fractions_named) {
    dP <- setNames(rep(0, length(iso)), iso)
    dP[names(fractions_named)] <- -fractions_named * P[names(fractions_named)]
    dP
}

# Single year: 10 percent US decline. This is the pipeline's PTA step.
message("Single-year PTA, United States -10 percent, alpha = ", alpha)
dP1 <- shock_of(c(USA = 0.10))
year1 <- sim_cascade_PTA(make_net(R0), dP1, rfrac = alpha, verbose = FALSE)
usa <- match("USA", names(year1$P))
check(is.finite(year1$P[usa]) && year1$P[usa] >= 0, "US production stays finite and non-negative")
check(abs(year1$P[usa] - 0.9 * P["USA"]) < 1e-4 * P["USA"], "US production falls by 10 percent")
check(all(is.finite(year1$C)) && all(year1$C >= -1e-6), "single-year consumption has no NaN and is non-negative")
check(all(is.finite(year1$E)) && all(year1$E >= -1e-8), "single-year exports are finite and non-negative")
check(all(year1$R >= -1e-6), "single-year usable reserves are non-negative")
check(all(abs(diag(year1$E)) == 0), "single-year diagonal is zero")
# Carry the physical stock, not the already-scaled usable reserve.
R_after_1 <- pmax(0, R0 + year1$dR)
check(all(is.finite(R_after_1)) && all(R_after_1 >= -1e-6), "carried reserves are non-negative")

# Second year on the same 2015-2017 network: a smaller US shock plus Ukraine.
message("Second year on the same baseline network, reserves carried forward")
dP2 <- shock_of(c(USA = 0.05, UKR = 0.20))
year2 <- sim_cascade_PTA(make_net(R_after_1), dP2, rfrac = alpha, verbose = FALSE)
check(year2$P["USA"] >= 0 && year2$P["UKR"] >= 0, "year-2 shocked production is non-negative")
check(abs(year2$P["UKR"] - 0.8 * P["UKR"]) < 1e-4 * max(P["UKR"], 1), "Ukraine production falls by 20 percent")
check(all(year2$C >= -1e-6) && all(is.finite(year2$C)), "year-2 consumption is finite and non-negative")
R_after_2 <- pmax(0, R_after_1 + year2$dR)
check(all(R_after_2 >= -1e-6), "year-2 reserves do not go negative")
# The same baseline production is shocked again. The year-1 loss is not left
# inside P, so it is not applied twice.
check(abs(year2$P["USA"] - 0.95 * P["USA"]) < 1e-4 * P["USA"],
      "year 2 shocks the baseline again rather than the already-reduced year-1 production")

# Static export bans read the scenario file and remove exactly the banned share.
# Ethiopia is in the wheat file; the old hard-coded block applied Pakistan twice
# and left a full ban as an unchanged export row.
wheat <- load_baseline("Wheat_Avg20152017")
ban_file <- "inputs/2021_C2P2_scenarios/Scenario_Wheat_ExportRestrictionFraction_Year2008_195countries.csv"
ban_df <- read.csv(ban_file, stringsAsFactors = FALSE)
ban_frac <- setNames(pmax(ban_df$year_1, 0), ban_df$iso3)
original_exports <- rowSums(wheat$E)
banned <- apply_export_bans(wheat$E, reserves = rep(0, nrow(wheat$E)), ban_frac)
check(all(abs(banned$E["ETH", ]) < 1e-6), "wheat export-ban file zeroes Ethiopia")
check(abs(sum(banned$E["PAK", ])) < 1e-6, "Pakistan's full ban is applied once")
check(abs(banned$reserves[match("ETH", rownames(wheat$E))] - original_exports["ETH"]) < 1e-4,
      "banned wheat exports are added to reserves once")
check(abs(sum(banned$E["KAZ", ]) - 0.83 * original_exports["KAZ"]) < 1e-2 * max(original_exports["KAZ"], 1),
      "a partial ban keeps one minus the banned share")

out_dir <- "outputs/smoke_test"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
summary_df <- data.frame(
    crop = crop,
    baseline = stem,
    alpha = alpha,
    countries = length(iso),
    baseline_negative_domestic = n_neg,
    usa_p0 = P["USA"],
    usa_p_year1 = year1$P["USA"],
    usa_p_year2 = year2$P["USA"],
    ukr_p_year2 = year2$P["UKR"],
    world_reserve_year0 = sum(R0),
    world_reserve_year1 = sum(R_after_1),
    world_reserve_year2 = sum(R_after_2),
    world_consumption_year0 = sum(pmax(domestic0, 0)),
    world_consumption_year1 = sum(year1$C),
    world_consumption_year2 = sum(year2$C),
    stringsAsFactors = FALSE
)
write.csv(summary_df, file.path(out_dir, paste0(crop, "_summary.csv")), row.names = FALSE)
country_df <- data.frame(
    iso3 = iso,
    P0 = P,
    C0 = domestic0,
    R0 = as.numeric(R0),
    P_year1 = as.numeric(year1$P),
    C_year1 = as.numeric(year1$C),
    R_year1 = as.numeric(R_after_1),
    dR_year1 = as.numeric(year1$dR),
    P_year2 = as.numeric(year2$P),
    C_year2 = as.numeric(year2$C),
    R_year2 = as.numeric(R_after_2),
    stringsAsFactors = FALSE
)
write.csv(country_df, file.path(out_dir, paste0(crop, "_countries.csv")), row.names = FALSE)

if (length(failures)) {
    stop(paste(c("Smoke test failed:", failures), collapse = "\n"))
}
message("Smoke test passed. Results are in ", out_dir)

# Yawei-aligned FSC benchmarks on the 2018-2021 metric-ton networks.
# Run from the repository root after export_riken_inputs.py:
#   Rscript main/run_yawei_benchmarks.R
#
# Experiment 3 reads inputs/yawei_placeholder_flood_shocks.csv. Replace the
# production_loss_fraction column with Junshi's flood shocks and re-run.
# The price coefficients are uncalibrated. beta_surplus and beta_stu are
# negative so a fall in exportable surplus or stock-to-use raises the index.
# They are not estimated Kuhla elasticities.

suppressPackageStartupMessages(library(dplyr))
source("main/FSC_component_funcs.R")
source("main/FSC_sim_funcs.R")
source("R/rice_price_step.R")

beta_surplus <- -1
beta_stu <- -0.5
alphas <- c(0, 0.25, 0.5, 1)
restrictions <- c(0.50, 0.95)
durations <- c(full = 1, six_month = 0.5)

wheat_exporters <- c("RUS", "CAN", "EU-28", "USA", "UKR", "AUS", "ARG", "KAZ", "PAK")
rice_exporters <- c("IND", "THA", "VNM", "PAK", "USA", "MMR")

eu_names <- c(
    "Austria", "Belgium", "Denmark", "Finland", "France", "Germany", "Greece",
    "Ireland", "Italy", "Luxembourg", "Netherlands", "Portugal", "Spain", "Sweden",
    "United Kingdom", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Estonia",
    "Hungary", "Latvia", "Lithuania", "Poland", "Romania", "Slovakia", "Slovenia"
)

regions <- list(
    Western_Africa = c("BEN", "BFA", "CPV", "CIV", "GMB", "GHA", "GIN", "GNB", "LBR", "MLI", "MRT", "NER", "NGA", "SEN", "SLE", "TGO"),
    Middle_Africa = c("AGO", "CMR", "CAF", "TCD", "COD", "COG", "GNQ", "GAB", "STP"),
    Eastern_Africa = c("BDI", "COM", "DJI", "ERI", "ETH", "KEN", "MDG", "MWI", "MUS", "MOZ", "RWA", "SYC", "SOM", "SSD", "UGA", "TZA", "ZMB", "ZWE"),
    GCC = c("BHR", "KWT", "OMN", "QAT", "SAU", "ARE"),
    South_Asia = c("AFG", "BGD", "BTN", "IND", "IRN", "MDV", "NPL", "PAK", "LKA"),
    Southeast_Asia = c("BRN", "KHM", "IDN", "LAO", "MYS", "MMR", "PHL", "SGP", "THA", "TLS", "VNM")
)

export_root <- "outputs/export_for_RIKEN"
out_dir <- "outputs/yawei_benchmarks"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

filled_stock <- function(beginning, imputed) {
    out <- beginning
    missing <- !is.finite(out)
    out[missing] <- imputed[missing]
    out[!is.finite(out)] <- 0
    out
}

read_crop <- function(crop, file_stem) {
    annual <- file.path(export_root, "annual_networks_2002_2024")
    base <- file.path(export_root, "baseline_2018_2021")
    # Baseline files are the 2018-2021 means' source rows, still with a year column.
    list(
        production = read.csv(file.path(base, sprintf("%s_production_2018_2021_mt.csv", crop))),
        reserves = read.csv(file.path(base, sprintf("%s_reserves_2018_2021_mt.csv", crop))),
        trade = read.csv(file.path(base, sprintf("%s_bilateral_trade_2018_2021_mt.csv", crop))),
        annual_production = read.csv(file.path(annual, sprintf("%s_production_2002_2024_mt.csv", crop))),
        annual_reserves = read.csv(file.path(annual, sprintf("%s_reserves_2002_2024_mt.csv", crop))),
        annual_trade = read.csv(file.path(annual, sprintf("%s_bilateral_trade_2002_2024_mt.csv", crop)))
    )
}

build_network <- function(bundle) {
    prod <- bundle$production
    res <- bundle$reserves
    trade <- bundle$trade
    iso <- sort(unique(prod$iso3))
    n_years <- length(unique(prod$year))
    P <- tapply(prod$production_mt, prod$iso3, sum) / n_years
    P <- P[iso]
    res$stock <- filled_stock(res$beginning_stocks_mt, res$reserves_imputed_mt)
    R <- tapply(res$stock, res$iso3, sum) / n_years
    R <- R[iso]
    E <- matrix(0, length(iso), length(iso), dimnames = list(iso, iso))
    keep <- trade$exporter_iso3 %in% iso & trade$importer_iso3 %in% iso
    trade <- trade[keep, , drop = FALSE]
    if (nrow(trade)) {
        summed <- aggregate(trade_mt_net ~ exporter_iso3 + importer_iso3, trade, sum)
        ri <- match(summed$exporter_iso3, iso)
        ci <- match(summed$importer_iso3, iso)
        E[cbind(ri, ci)] <- summed$trade_mt_net / n_years
    }
    diag(E) <- 0
    C <- as.numeric(P + colSums(E) - rowSums(E))
    names(C) <- iso
    list(
        P = P, R = R, E = E, C = C,
        dR = setNames(rep(0, length(iso)), iso),
        shortage = setNames(rep(0, length(iso)), iso),
        nc = length(iso),
        iso = iso
    )
}

ban_targets <- function(label, iso, eu_iso) {
    if (label == "EU-28") {
        hit <- intersect(eu_iso, iso)
        if (!length(hit)) stop("EU-28 members are missing from the network")
        return(hit)
    }
    if (!label %in% iso) stop("Exporter not in the network: ", label)
    label
}

run_case <- function(net, targets, annual_fraction, alpha, dP = NULL,
                     model = "PTA", freeze_empty_reserves = TRUE) {
    frac <- setNames(rep(0, net$nc), net$iso)
    frac[targets] <- annual_fraction
    banned <- apply_export_bans(net$E, net$R, frac)
    shocked <- net
    shocked$E <- banned$E
    shocked$R <- banned$reserves
    # The withdrawn exports are a stock build. dR enters supply with a minus,
    # so a positive dR keeps that grain out of current supply and avoids a
    # negative shortage at the exporter.
    removed <- as.numeric(banned$removed)
    names(removed) <- net$iso
    shocked$dR <- net$dR + removed
    freeze <- setNames(rep(FALSE, net$nc), net$iso)
    freeze[targets] <- annual_fraction > 0
    if (is.null(dP)) dP <- setNames(rep(0, net$nc), net$iso)
    result <- tryCatch(
        if (identical(model, "RTA")) {
            sim_cascade_RTA(shocked, dP, alpha, verbose = FALSE, trade_freeze = freeze)
        } else {
            sim_cascade_PTA(
                shocked, dP, alpha, verbose = FALSE, trade_freeze = freeze,
                freeze_empty_reserves = freeze_empty_reserves
            )
        },
        error = function(e) e
    )
    if (inherits(result, "error")) {
        return(list(ok = FALSE, message = conditionMessage(result)))
    }
    if (is.null(names(result$P)) || is.null(names(result$C)) || is.null(names(result$R))) {
        names(result$P) <- net$iso
        names(result$C) <- net$iso
        names(result$R) <- net$iso
    }
    if (is.null(rownames(result$E))) {
        dimnames(result$E) <- list(net$iso, net$iso)
    }
    list(ok = TRUE, net = result, removed = banned$removed)
}

region_metrics <- function(iso, C0, C1) {
    decline <- pmax(C0, 0) - pmax(C1, 0)
    decline[!is.finite(decline)] <- NA_real_
    rows <- lapply(names(regions), function(name) {
        members <- intersect(regions[[name]], iso)
        base <- sum(pmax(C0[members], 0), na.rm = TRUE)
        loss <- sum(pmax(decline[members], 0), na.rm = TRUE)
        data.frame(
            region = name,
            baseline_consumption_mt = base,
            consumption_loss_mt = loss,
            loss_fraction = if (base > 0) loss / base else NA_real_,
            stringsAsFactors = FALSE
        )
    })
    world_base <- sum(pmax(C0, 0), na.rm = TRUE)
    world_loss <- sum(pmax(decline, 0), na.rm = TRUE)
    rbind(
        do.call(rbind, rows),
        data.frame(
            region = "World",
            baseline_consumption_mt = world_base,
            consumption_loss_mt = world_loss,
            loss_fraction = if (world_base > 0) world_loss / world_base else NA_real_,
            stringsAsFactors = FALSE
        )
    )
}

price_panel <- function(bundle) {
    prod <- bundle$annual_production
    prod <- prod[prod$year %in% 2018:2021, , drop = FALSE]
    res <- bundle$annual_reserves
    res <- res[res$year %in% 2018:2021, , drop = FALSE]
    trade <- bundle$annual_trade
    trade <- trade[trade$year %in% 2018:2021, , drop = FALSE]
    exports <- aggregate(trade_mt_net ~ exporter_iso3 + year, trade, sum)
    names(exports)[1] <- "iso3"
    panel <- merge(prod[, c("iso3", "year", "production_mt")],
                   res[, c("iso3", "year", "domestic_use_mt", "beginning_stocks_mt", "reserves_imputed_mt", "PSD_Missing_Flag")],
                   by = c("iso3", "year"), all.x = TRUE)
    panel <- merge(panel, exports, by = c("iso3", "year"), all.x = TRUE)
    names(panel)[names(panel) == "trade_mt_net"] <- "exports_mt"
    panel$exports_mt[!is.finite(panel$exports_mt)] <- 0
    panel$beginning_stocks_mt <- filled_stock(panel$beginning_stocks_mt, panel$reserves_imputed_mt)
    panel$domestic_use_mt[!is.finite(panel$domestic_use_mt)] <- 0
    panel$PSD_Missing_Flag <- 0L
    panel
}

scenario_price <- function(panel, final_net) {
    iso <- final_net$iso
    scen <- data.frame(
        iso3 = iso,
        year = 2025L,
        production_mt = as.numeric(final_net$P[iso]),
        domestic_use_mt = pmax(as.numeric(final_net$C[iso]), 0),
        beginning_stocks_mt = pmax(as.numeric(final_net$R[iso]), 0),
        exports_mt = as.numeric(rowSums(final_net$E)[iso]),
        reserves_imputed_mt = NA_real_,
        PSD_Missing_Flag = 0L,
        stringsAsFactors = FALSE
    )
    fit <- rice_price_step(
        rbind(panel, scen),
        baseline_years = 2018:2021,
        beta_surplus = beta_surplus,
        beta_stu = beta_stu,
        use_imputed = FALSE
    )
    row <- fit$world[fit$world$year == 2025L, ]
    if (!nrow(row)) {
        return(list(rice_price_index = NA_real_, n_price_exporters = 0L))
    }
    list(rice_price_index = row$rice_price_index, n_price_exporters = row$n_exporters)
}

lookup <- read.csv(file.path(export_root, "country_lookup_ISO3_FAO.csv"), stringsAsFactors = FALSE)
eu_iso <- lookup$iso3[lookup$country_name %in% eu_names]
if (length(eu_iso) != length(eu_names)) {
    stop("Could not map every EU-28 name onto the country lookup")
}

message("Loading 2018-2021 networks")
bundles <- list(
    wheat = read_crop("wheat"),
    rice_milled = read_crop("rice_milled"),
    maize = read_crop("maize")
)
nets <- lapply(bundles, build_network)
rice_prices <- price_panel(bundles$rice_milled)

records <- list()
add_record <- function(row) {
    records[[length(records) + 1L]] <<- row
}

counterfactual_block <- function(experiment, crop, exporters) {
    net <- nets[[crop]]
    bundle_panel <- if (crop == "rice_milled") rice_prices else NULL
    for (exporter in exporters) {
        targets <- ban_targets(exporter, net$iso, eu_iso)
        for (restriction in restrictions) {
            for (duration_i in seq_along(durations)) {
                duration <- names(durations)[duration_i]
                annual_fraction <- unname(restriction * durations[[duration_i]])
                for (alpha in alphas) {
                    message(sprintf(
                        "%s %s %s restriction %.2f %s alpha %.2f",
                        experiment, crop, exporter, restriction, duration, alpha
                    ))
                    ran <- run_case(net, targets, annual_fraction, alpha)
                    metrics <- if (isTRUE(ran$ok)) {
                        region_metrics(net$iso, net$C, ran$net$C)
                    } else {
                        NULL
                    }
                    price <- list(rice_price_index = NA_real_, n_price_exporters = NA_integer_)
                    if (isTRUE(ran$ok) && crop == "rice_milled") {
                        final <- ran$net
                        final$iso <- net$iso
                        price <- scenario_price(bundle_panel, final)
                    }
                    base_row <- data.frame(
                        experiment = experiment,
                        crop = crop,
                        exporter = exporter,
                        event_id = "",
                        event_label = "",
                        event_year = NA_integer_,
                        variant = "export_ban",
                        production_loss_fraction = 0,
                        restriction = restriction,
                        duration = duration,
                        annual_fraction = annual_fraction,
                        alpha = alpha,
                        ok = isTRUE(ran$ok),
                        error = if (isTRUE(ran$ok)) "" else ran$message,
                        rice_price_index = price$rice_price_index,
                        n_price_exporters = price$n_price_exporters,
                        beta_surplus = if (crop == "rice_milled") beta_surplus else NA_real_,
                        beta_stu = if (crop == "rice_milled") beta_stu else NA_real_,
                        shock_source = "export_restriction",
                        stringsAsFactors = FALSE
                    )
                    if (is.null(metrics)) {
                        add_record(cbind(base_row, data.frame(
                            region = NA_character_, baseline_consumption_mt = NA_real_,
                            consumption_loss_mt = NA_real_, loss_fraction = NA_real_,
                            stringsAsFactors = FALSE
                        )))
                    } else {
                        add_record(cbind(base_row, metrics, row.names = NULL))
                    }
                }
            }
        }
    }
}

if (!identical(Sys.getenv("YAWEI_SOURCE_ONLY"), "1")) {
counterfactual_block("wheat_exporter", "wheat", wheat_exporters)
counterfactual_block("rice_exporter", "rice_milled", rice_exporters)

shocks <- read.csv("inputs/yawei_placeholder_flood_shocks.csv", stringsAsFactors = FALSE)
for (i in seq_len(nrow(shocks))) {
    shock <- shocks[i, ]
    net <- nets[[shock$crop]]
    if (is.null(net)) stop("No network for ", shock$crop)
    dP <- setNames(rep(0, net$nc), net$iso)
    dP[shock$iso3] <- -shock$production_loss_fraction * net$P[shock$iso3]
    for (variant in c("production_only", "production_plus_ban")) {
        ban <- if (variant == "production_plus_ban") shock$reactive_ban_fraction else 0
        for (alpha in alphas) {
            message(sprintf("flood %s %s alpha %.2f", shock$event_id, variant, alpha))
            ran <- run_case(net, shock$iso3, ban, alpha, dP = dP)
            metrics <- if (isTRUE(ran$ok)) region_metrics(net$iso, net$C, ran$net$C) else NULL
            price <- list(rice_price_index = NA_real_, n_price_exporters = NA_integer_)
            if (isTRUE(ran$ok) && shock$crop == "rice_milled") {
                final <- ran$net
                final$iso <- net$iso
                price <- scenario_price(rice_prices, final)
            }
            base_row <- data.frame(
                experiment = "flood_policy",
                crop = shock$crop,
                exporter = shock$iso3,
                event_id = shock$event_id,
                event_label = shock$event_label,
                event_year = shock$year,
                variant = variant,
                production_loss_fraction = shock$production_loss_fraction,
                restriction = ban,
                duration = "full",
                annual_fraction = ban,
                alpha = alpha,
                ok = isTRUE(ran$ok),
                error = if (isTRUE(ran$ok)) "" else ran$message,
                rice_price_index = price$rice_price_index,
                n_price_exporters = price$n_price_exporters,
                beta_surplus = if (shock$crop == "rice_milled") beta_surplus else NA_real_,
                beta_stu = if (shock$crop == "rice_milled") beta_stu else NA_real_,
                shock_source = shock$shock_source,
                stringsAsFactors = FALSE
            )
            if (is.null(metrics)) {
                add_record(cbind(base_row, data.frame(
                    region = NA_character_, baseline_consumption_mt = NA_real_,
                    consumption_loss_mt = NA_real_, loss_fraction = NA_real_,
                    stringsAsFactors = FALSE
                )))
            } else {
                add_record(cbind(base_row, metrics, row.names = NULL))
            }
        }
    }
}

results <- do.call(rbind, records)
rownames(results) <- NULL
write.csv(results, file.path(out_dir, "benchmark_results.csv"), row.names = FALSE)

highlight <- results[results$ok & results$region %in% c(names(regions), "World"), ]
# Concise slice: full-year 95% bans at alpha 0.5, and flood variants at alpha 0.5.
slice <- highlight[
    highlight$alpha == 0.5 &
        (
            (highlight$experiment %in% c("wheat_exporter", "rice_exporter") &
                highlight$restriction == 0.95 & highlight$duration == "full") |
            highlight$experiment == "flood_policy"
        ),
]
write.csv(slice, file.path(out_dir, "summary_alpha0.5.csv"), row.names = FALSE)

print_block <- function(title, block) {
    cat("\n", title, "\n", sep = "")
    if (!nrow(block)) {
        cat("(no rows)\n")
        return(invisible(NULL))
    }
    regions_show <- c(names(regions), "World")
    wide <- reshape(
        block[, c("label", "region", "loss_fraction")],
        idvar = "label",
        timevar = "region",
        direction = "wide"
    )
    prices <- unique(block[, c("label", "rice_price_index")])
    wide <- merge(prices, wide, by = "label", all.x = TRUE)
    names(wide) <- sub("^loss_fraction\\.", "", names(wide))
    cols <- c("label", "rice_price_index", regions_show)
    cols <- cols[cols %in% names(wide)]
    wide <- wide[, cols, drop = FALSE]
    for (col in setdiff(cols, "label")) {
        wide[[col]] <- round(wide[[col]], 4)
    }
    print(wide, row.names = FALSE)
}

wheat_slice <- slice[slice$experiment == "wheat_exporter", ]
wheat_slice$label <- wheat_slice$exporter
print_block("Wheat 95% full-year export ban, alpha 0.5. Cells are regional consumption-loss fractions.", wheat_slice)

rice_slice <- slice[slice$experiment == "rice_exporter", ]
rice_slice$label <- rice_slice$exporter
print_block("Rice 95% full-year export ban, alpha 0.5. Price index uses beta_surplus -1 and beta_stu -0.5.", rice_slice)

flood_slice <- slice[slice$experiment == "flood_policy", ]
flood_slice$label <- paste(flood_slice$event_id, flood_slice$variant, flood_slice$crop)
print_block("Placeholder flood shocks, alpha 0.5. Replace inputs/yawei_placeholder_flood_shocks.csv with Junshi's fractions.", flood_slice)

message("Wrote ", file.path(out_dir, "benchmark_results.csv"))
}

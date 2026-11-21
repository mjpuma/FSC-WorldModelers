# Run the flood-shock table on each year's own network.
# From the repository root:
#   Rscript main/run_flood_shocks.R
#
# Primary rule: stock-then-split. Sensitivity: modified PTA, the published PTA
# update with the empty-reserve freeze off. Alpha is 0 or 1. Missing beginning
# stocks are zero. The applied loss is shock_total_t, which already includes
# the second-year term. The 2018-2021 mean is not used.

suppressPackageStartupMessages(library(dplyr))
source("main/FSC_component_funcs.R")
source("main/FSC_sim_funcs.R")

shock_path <- "data/RIKEN_flood_shock_table_2002_2024_draft_2026-11-20.xlsx"
annual_dir <- "outputs/export_for_RIKEN/annual_networks_2002_2024"
out_dir <- "outputs/flood_shocks"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

crops <- c("rice_milled", "wheat", "maize")
alphas <- c(0, 1)
rules <- list(
    list(allocation_rule = "stock_then_split", kind = "split"),
    list(allocation_rule = "modified_pta", kind = "pta")
)

read_shock_table <- function(path) {
    py <- tempfile(fileext = ".py")
    dest <- tempfile(fileext = ".csv")
    writeLines(c(
        "import pandas as pd",
        sprintf("pd.read_excel(r'%s', sheet_name='shocks').to_csv(r'%s', index=False)", path, dest)
    ), py)
    status <- system2("python3", py)
    if (!identical(status, 0L)) stop("Could not read the shock workbook")
    utils::read.csv(dest, stringsAsFactors = FALSE)
}

message("Reading shock table")
shocks <- read_shock_table(shock_path)
needed <- c("crop", "iso3", "year", "shock_y0_t", "shock_y1_t", "shock_total_t", "production_mt", "capped_at_production")
missing <- setdiff(needed, names(shocks))
if (length(missing)) stop("Shock table is missing columns: ", paste(missing, collapse = ", "))
if (any(!shocks$crop %in% crops)) stop("Unexpected crop in the shock table")
if (any(shocks$shock_total_t < 0)) stop("Negative shock_total_t")
shocks$capped_at_production <- tolower(as.character(shocks$capped_at_production)) %in% c("true", "1", "yes")
gap <- abs(shocks$shock_y0_t + shocks$shock_y1_t - shocks$shock_total_t)
if (any(gap > 0.2)) stop("shock_total_t is not the sum of y0 and y1")

read_annual <- function(crop) {
    list(
        production = read.csv(file.path(annual_dir, sprintf("%s_production_2002_2024_mt.csv", crop)), stringsAsFactors = FALSE),
        reserves = read.csv(file.path(annual_dir, sprintf("%s_reserves_2002_2024_mt.csv", crop)), stringsAsFactors = FALSE),
        trade = read.csv(file.path(annual_dir, sprintf("%s_bilateral_trade_2002_2024_mt.csv", crop)), stringsAsFactors = FALSE)
    )
}

build_year_network <- function(bundle, year) {
    prod <- bundle$production[bundle$production$year == year, , drop = FALSE]
    res <- bundle$reserves[bundle$reserves$year == year, , drop = FALSE]
    trade <- bundle$trade[bundle$trade$year == year, , drop = FALSE]
    iso <- sort(unique(prod$iso3))
    P <- setNames(prod$production_mt, prod$iso3)[iso]
    P[!is.finite(P)] <- 0
    stock <- res$beginning_stocks_mt
    stock[!is.finite(stock)] <- 0
    names(stock) <- res$iso3
    R <- stock[iso]
    R[!is.finite(R)] <- 0
    E <- matrix(0, length(iso), length(iso), dimnames = list(iso, iso))
    keep <- trade$exporter_iso3 %in% iso & trade$importer_iso3 %in% iso
    trade <- trade[keep, , drop = FALSE]
    if (nrow(trade)) {
        summed <- aggregate(trade_mt_net ~ exporter_iso3 + importer_iso3, trade, sum)
        ri <- match(summed$exporter_iso3, iso)
        ci <- match(summed$importer_iso3, iso)
        E[cbind(ri, ci)] <- summed$trade_mt_net
    }
    diag(E) <- 0
    C <- as.numeric(P + colSums(E) - rowSums(E))
    names(C) <- iso
    list(
        P = P, R = R, E = E, C = C,
        dR = setNames(rep(0, length(iso)), iso),
        shortage = setNames(rep(0, length(iso)), iso),
        nc = length(iso), iso = iso
    )
}

bundles <- setNames(lapply(crops, read_annual), crops)

prod_gap <- sapply(seq_len(nrow(shocks)), function(i) {
    row <- shocks[i, ]
    hit <- bundles[[row$crop]]$production
    hit <- hit$production_mt[hit$iso3 == row$iso3 & hit$year == row$year]
    if (!length(hit)) return(Inf)
    abs(hit[1] - row$production_mt)
})
if (any(prod_gap > 1)) stop("Shock-table production does not match the annual network")
unknown <- setdiff(unique(shocks$iso3), bundles$wheat$production$iso3)
if (length(unknown)) stop("Shock ISO3 not in the network: ", paste(unknown, collapse = ", "))

run_one <- function(net, dP, alpha, kind) {
    warns <- character()
    result <- withCallingHandlers(
        if (kind == "split") {
            sim_cascade_split(net, dP, rfrac = alpha, verbose = FALSE)
        } else {
            sim_cascade_PTA(
                net, dP, rfrac = alpha, verbose = FALSE,
                freeze_empty_reserves = FALSE
            )
        },
        warning = function(w) {
            warns <<- c(warns, conditionMessage(w))
            invokeRestart("muffleWarning")
        }
    )
    list(net = result, warnings = warns)
}

summarize <- function(net, final, dP, alpha, shocked) {
    C0_raw <- as.numeric(net$C)
    C1_raw <- as.numeric(final$C[net$iso])
    names(C0_raw) <- net$iso
    names(C1_raw) <- net$iso
    C0 <- pmax(C0_raw, 0)
    C1 <- pmax(C1_raw, 0)
    loss <- pmax(C0 - C1, 0)
    P0 <- as.numeric(net$P)
    P1 <- as.numeric(final$P[net$iso])
    dR <- as.numeric(final$dR[net$iso])
    E1 <- final$E
    dimnames(E1) <- dimnames(net$E)
    R_open <- alpha * pmax(as.numeric(net$R), 0)
    R_final <- pmax(as.numeric(final$R[net$iso]), 0)
    drawn <- R_open - R_final
    names(drawn) <- net$iso
    direct <- loss[shocked]
    data.frame(
        production_loss_mt = -sum(dP),
        net_consumption_change_mt = sum(C1_raw - C0_raw),
        world_consumption_loss_mt = sum(loss),
        direct_consumption_loss_mt = sum(direct),
        direct_loss_share = if (sum(loss) > 0) sum(direct) / sum(loss) else NA_real_,
        abroad_consumption_loss_mt = sum(loss) - sum(direct),
        reserve_draw_mt = sum(pmax(drawn, 0)),
        direct_reserve_draw_mt = sum(pmax(drawn[shocked], 0)),
        stock_change_mt = sum(dR),
        mass_gap_mt = sum(C1_raw - C0_raw) - sum(P1 - P0) + sum(dR),
        neg_production = sum(P1 < -1e-6),
        neg_reserves = sum(R_final < -1e-6),
        neg_trade = sum(E1 < -1e-6),
        neg_use = sum(C1_raw < -1 & C0_raw >= -1),
        new_links = as.integer(sum(net$E <= 1e-8 & E1 > 1)),
        reserve_overdraw = isTRUE(any(drawn > R_open + 1e-2)),
        iterations = as.integer(final$cascade_iterations),
        converged = isTRUE(final$cascade_converged),
        stringsAsFactors = FALSE
    )
}

rows <- list()
country_rows <- list()
edge_rows <- list()
pairs <- unique(shocks[, c("crop", "year")])
pairs <- pairs[order(pairs$crop, pairs$year), ]
message(sprintf("Crop-years: %d. Rules: 2. Alpha: 0 and 1.", nrow(pairs)))
for (i in seq_len(nrow(pairs))) {
    crop <- pairs$crop[i]
    year <- pairs$year[i]
    block <- shocks[shocks$crop == crop & shocks$year == year, ]
    net <- build_year_network(bundles[[crop]], year)
    dP <- setNames(rep(0, net$nc), net$iso)
    present <- block$iso3 %in% net$iso
    if (!all(present)) stop("Missing country in ", crop, " ", year)
    dP[block$iso3] <- -block$shock_total_t
    for (alpha in alphas) {
        for (rule in rules) {
            message(sprintf("%s %s alpha %s %s", crop, year, alpha, rule$allocation_rule))
            ran <- run_one(net, dP, alpha, rule$kind)
            final <- ran$net
            summary_row <- data.frame(
                crop = crop,
                year = year,
                baseline = sprintf("year_%s", year),
                allocation_rule = rule$allocation_rule,
                alpha = alpha,
                policy_variant = "production_only",
                shock_source = "flood_shock_table_draft_2026-11-20",
                n_shocked_countries = nrow(block),
                shock_y0_mt = sum(block$shock_y0_t),
                shock_y1_mt = sum(block$shock_y1_t),
                n_capped = sum(as.logical(block$capped_at_production)),
                n_warnings = length(ran$warnings),
                warning_text = if (length(ran$warnings)) paste(unique(ran$warnings), collapse = " | ") else "",
                stringsAsFactors = FALSE
            )
            rows[[length(rows) + 1L]] <- cbind(summary_row, summarize(net, final, dP, alpha, block$iso3))
            C0_raw <- as.numeric(net$C)
            C1_raw <- as.numeric(final$C[net$iso])
            names(C0_raw) <- net$iso
            names(C1_raw) <- net$iso
            loss <- pmax(pmax(C0_raw, 0) - pmax(C1_raw, 0), 0)
            R0 <- pmax(as.numeric(net$R), 0)
            names(R0) <- net$iso
            R_final <- pmax(as.numeric(final$R[net$iso]), 0)
            drawn <- pmax(alpha * R0 - R_final, 0)
            names(drawn) <- net$iso
            dP_here <- -dP
            keep <- which(loss > 1000 | drawn > 1000 | net$iso %in% block$iso3)
            use_pos <- pmax(C0_raw[keep], 0)
            country_rows[[length(country_rows) + 1L]] <- data.frame(
                crop = crop, year = year, allocation_rule = rule$allocation_rule,
                alpha = alpha, iso3 = net$iso[keep],
                directly_shocked = net$iso[keep] %in% block$iso3,
                production_mt = as.numeric(net$P[net$iso[keep]]),
                production_loss_mt = as.numeric(dP_here[net$iso[keep]]),
                beginning_stocks_mt = as.numeric(R0[keep]),
                baseline_use_mt = as.numeric(C0_raw[keep]),
                stu_network = ifelse(use_pos > 1, as.numeric(R0[keep]) / use_pos, NA_real_),
                consumption_loss_mt = as.numeric(loss[keep]),
                reserve_draw_mt = as.numeric(drawn[keep]),
                stringsAsFactors = FALSE
            )
            E1 <- final$E
            dimnames(E1) <- dimnames(net$E)
            changed <- which(abs(net$E - E1) > 1000, arr.ind = TRUE)
            if (nrow(changed)) {
                edge_rows[[length(edge_rows) + 1L]] <- data.frame(
                    crop = crop, year = year, allocation_rule = rule$allocation_rule,
                    alpha = alpha,
                    exporter_iso3 = rownames(net$E)[changed[, 1]],
                    importer_iso3 = colnames(net$E)[changed[, 2]],
                    trade_t0_mt = net$E[changed],
                    trade_t1_mt = E1[changed],
                    export_cut_mt = net$E[changed] - E1[changed],
                    stringsAsFactors = FALSE
                )
            }
        }
    }
}

summary_df <- do.call(rbind, rows)
country_df <- do.call(rbind, country_rows)
edge_df <- if (length(edge_rows)) do.call(rbind, edge_rows) else data.frame()
write.csv(summary_df, file.path(out_dir, "flood_shock_summary.csv"), row.names = FALSE)
write.csv(country_df, file.path(out_dir, "flood_shock_countries.csv"), row.names = FALSE)
write.csv(edge_df, file.path(out_dir, "flood_shock_edges.csv"), row.names = FALSE)
write.csv(shocks, file.path(out_dir, "shock_table_used.csv"), row.names = FALSE)

by_rule <- function(rule, alpha) {
    hit <- summary_df[summary_df$allocation_rule == rule & summary_df$alpha == alpha, ]
    data.frame(
        rule = rule, alpha = alpha,
        production_loss_mt = sum(hit$production_loss_mt),
        world_loss_mt = sum(hit$world_consumption_loss_mt),
        direct_loss_mt = sum(hit$direct_consumption_loss_mt),
        direct_share = sum(hit$direct_consumption_loss_mt) / sum(hit$world_consumption_loss_mt),
        reserve_draw_mt = sum(hit$reserve_draw_mt),
        stringsAsFactors = FALSE
    )
}
totals <- rbind(
    by_rule("stock_then_split", 0), by_rule("stock_then_split", 1),
    by_rule("modified_pta", 0), by_rule("modified_pta", 1)
)
crop_lines <- c(
    "| Crop | Production loss | Split, no stocks, share in shocked countries | Split, stocks open, share of the loss drawn from stocks | Split, stocks open, remaining consumption loss | Modified PTA, no stocks, share in shocked countries |",
    "|---|---:|---:|---:|---:|---:|"
)
for (crop in crops) {
    no_stock <- summary_df[summary_df$crop == crop & summary_df$allocation_rule == "stock_then_split" & summary_df$alpha == 0, ]
    stocks <- summary_df[summary_df$crop == crop & summary_df$allocation_rule == "stock_then_split" & summary_df$alpha == 1, ]
    pta <- summary_df[summary_df$crop == crop & summary_df$allocation_rule == "modified_pta" & summary_df$alpha == 0, ]
    share <- function(hit) {
        total <- sum(hit$world_consumption_loss_mt)
        if (total <= 0) return("n/a")
        sprintf("%.1f%%", 100 * sum(hit$direct_consumption_loss_mt) / total)
    }
    crop_lines <- c(crop_lines, sprintf(
        "| %s | %.2f | %s | %.1f%% | %.2f | %s |",
        crop,
        sum(no_stock$production_loss_mt) / 1e6,
        share(no_stock),
        100 * sum(stocks$reserve_draw_mt) / sum(stocks$production_loss_mt),
        -sum(stocks$net_consumption_change_mt) / 1e6,
        share(pta)
    ))
}
n_ok <- sum(summary_df$converged)
n_new <- sum(summary_df$new_links)
n_neg <- sum(summary_df$neg_production + summary_df$neg_reserves + summary_df$neg_trade + summary_df$neg_use)
note <- c(
    "# Flood shocks on each year's network",
    "",
    "Draft shock table dated 2026-11-20. Rows for 2015-2024 are provisional and are expected to be replaced. Losses are metric tons. Rice is already milled. The applied shock is shock_total_t, the same-year loss plus the second-year loss from the previous year's floods.",
    "",
    "Each crop-year uses that year's production, trade_mt_net, and USDA beginning stocks. Blank beginning stocks are zero. Imputed stocks are not used. Alpha 0 draws no stocks. Alpha 1 may draw the reported beginning stock. There is no price step and no export ban.",
    "",
    "Stock-then-split is the primary rule. Modified PTA, with the empty-reserve freeze off, is the sensitivity in which domestic use is defended. The published FSC default is unchanged.",
    "",
    sprintf(
        "Runs: %d. Converged: %d. New bilateral links: %d. Negative quantities: %d. Reserve overdraws: %d. Largest absolute mass gap: %.3f Mt.",
        nrow(summary_df), n_ok, n_new, n_neg, sum(summary_df$reserve_overdraw),
        max(abs(summary_df$mass_gap_mt)) / 1e6
    ),
    "",
    "Figures below are physical availability. They are not household consumption or nutritional outcomes. The share is the part of the consumption decline that sits in the countries hit by the flood. With stocks open, the stock share is the part of the production loss taken from beginning stocks.",
    "",
    crop_lines,
    "",
    sprintf(
        "Across all crop-years, with no stocks, stock-then-split puts %.1f%% of the consumption decline in the directly shocked countries. Modified PTA puts %.1f%% there. With beginning stocks open, stock-then-split draws %.2f Mt of a %.2f Mt production loss, and world consumption falls by %.2f Mt.",
        100 * totals$direct_share[1], 100 * totals$direct_share[3],
        totals$reserve_draw_mt[2] / 1e6, totals$production_loss_mt[2] / 1e6,
        -sum(summary_df$net_consumption_change_mt[summary_df$allocation_rule == "stock_then_split" & summary_df$alpha == 1]) / 1e6
    ),
    "",
    "With beginning stocks open, the loss is mostly absorbed by stocks for all three crops, and what remains stays in the flooded countries. With no stocks, rice and maize keep about 90 percent of the decline in the flooded countries. Wheat keeps 76 percent. Wheat in 2010 and 2011 keeps about half, so the claim that the loss mostly stays at home is weaker for wheat when stocks cannot be drawn.",
    "",
    sprintf(
        "Modified PTA with no stocks reached the 50-iteration cap in %d of %d crop-years, so its shares are approximate. The stock-then-split runs all converged and opened no new links. The largest mass gap in the split runs is %.0f tonnes. The larger gap in the line above is from modified PTA.",
        sum(!summary_df$converged & summary_df$allocation_rule == "modified_pta" & summary_df$alpha == 0),
        sum(summary_df$allocation_rule == "modified_pta" & summary_df$alpha == 0),
        max(abs(summary_df$mass_gap_mt[summary_df$allocation_rule == "stock_then_split"]))
    ),
    "",
    "The three rows capped at production are Papua New Guinea 2003 rice and maize, and Turkmenistan 2016 maize. The cap was already in the table.",
    ""
)
writeLines(note, file.path(out_dir, "FLOOD_SHOCK_RUNS.md"))
message("Wrote ", file.path(out_dir, "flood_shock_summary.csv"))

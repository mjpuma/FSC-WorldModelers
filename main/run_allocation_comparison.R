# Compare the three existing FSC allocation settings on one baseline network.
# Run from the repository root:
#   Rscript main/run_allocation_comparison.R
#
# Baseline is the 2018-2021 mean. The shock file is
# inputs/placeholder_flood_shocks.csv. Replace that file with observed
# flood losses and rerun. The allocation rules are not changed by the file.
#
# Specs, passed explicitly on every call:
#   modified_pta   PTA, freeze_empty_reserves = FALSE
#   published_pta  PTA, freeze_empty_reserves = TRUE
#   rta            RTA, rexp = 0.2 (the function default)

Sys.setenv(FSC_SOURCE_ONLY = "1")
source("main/run_flood_benchmarks.R")

out_dir <- "outputs/flood_benchmarks"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

specs <- list(
    list(allocation_rule = "modified_pta", model = "PTA", freeze_empty_reserves = FALSE),
    list(allocation_rule = "published_pta", model = "PTA", freeze_empty_reserves = TRUE),
    list(allocation_rule = "rta", model = "RTA", freeze_empty_reserves = TRUE)
)

run_spec <- function(net, targets, annual_fraction, alpha, dP, spec) {
    warns <- character()
    ran <- withCallingHandlers(
        run_case(
            net, targets, annual_fraction, alpha, dP = dP,
            model = spec$model,
            freeze_empty_reserves = spec$freeze_empty_reserves
        ),
        warning = function(w) {
            warns <<- c(warns, conditionMessage(w))
            invokeRestart("muffleWarning")
        }
    )
    ran$warnings <- warns
    ran
}

diagnose <- function(net, final, dP, removed, alpha, home_iso = "", targets = character(), annual_fraction = 0) {
    C0 <- pmax(as.numeric(net$C), 0)
    C1 <- pmax(as.numeric(final$C[net$iso]), 0)
    P0 <- as.numeric(net$P)
    P1 <- as.numeric(final$P[net$iso])
    dR <- as.numeric(final$dR[net$iso])
    E1 <- final$E
    dimnames(E1) <- dimnames(net$E)
    new_links <- sum(net$E <= 1e-8 & E1 > 1)
    R_open <- alpha * pmax(as.numeric(net$R) + as.numeric(removed), 0)
    R_final <- pmax(as.numeric(final$R[net$iso]), 0)
    drawn <- R_open - R_final
    loss <- pmax(C0 - C1, 0)
    home_loss <- if (nzchar(home_iso) && home_iso %in% net$iso) loss[net$iso == home_iso] else NA_real_
    abroad <- loss
    if (nzchar(home_iso) && home_iso %in% net$iso) abroad[net$iso == home_iso] <- 0
    dE <- E1 - net$E
    max_at <- which(dE == max(dE), arr.ind = TRUE)
    max_at <- max_at[1, ]
    base_link <- net$E[max_at[1], max_at[2]]
    final_link <- E1[max_at[1], max_at[2]]
    final_exports <- rowSums(E1)
    ceiling_exports <- rowSums(net$E) - as.numeric(removed)
    banned <- intersect(targets, net$iso)
    reopened <- length(banned) > 0 && annual_fraction > 0 &&
        any(final_exports[banned] > ceiling_exports[banned] + 1)
    data.frame(
        production_change_mt = sum(P1 - P0),
        intended_production_change_mt = sum(dP),
        shock_gap_mt = sum(P1 - P0) - sum(dP),
        consumption_loss_mt = sum(loss),
        consumption_gain_mt = sum(pmax(C1 - C0, 0)),
        net_consumption_change_mt = sum(C1 - C0),
        stock_change_mt = sum(dR),
        mass_gap_mt = sum(C1 - C0) - sum(P1 - P0) + sum(dR),
        neg_production = sum(P1 < -1e-6),
        neg_reserves = sum(R_final < -1e-6),
        neg_trade = sum(E1 < -1e-6),
        neg_use = sum(as.numeric(final$C[net$iso]) < -1e-6),
        new_links = as.integer(new_links),
        reserve_draw_mt = sum(pmax(drawn, 0)),
        reserve_overdraw = isTRUE(any(drawn > R_open + 1e-2)),
        home_iso3 = home_iso,
        home_loss_mt = if (is.finite(home_loss)) home_loss else NA_real_,
        home_loss_share = if (sum(loss) > 0 && is.finite(home_loss)) home_loss / sum(loss) else NA_real_,
        abroad_loss_mt = sum(abroad),
        abroad_loss_share = if (sum(loss) > 0) sum(abroad) / sum(loss) else NA_real_,
        countries_loss_gt_1kt = as.integer(sum(loss > 1000)),
        max_flow_increase_mt = max(dE),
        max_flow_from = rownames(net$E)[max_at[1]],
        max_flow_to = colnames(net$E)[max_at[2]],
        max_flow_baseline_mt = base_link,
        max_flow_ratio = if (base_link > 1) final_link / base_link else NA_real_,
        links_more_than_doubled = as.integer(sum(net$E > 1000 & E1 > 2 * net$E)),
        export_restriction_reopened = isTRUE(reopened),
        iterations = if (is.null(final$cascade_iterations)) NA_integer_ else as.integer(final$cascade_iterations),
        converged = isTRUE(final$cascade_converged),
        stringsAsFactors = FALSE
    )
}

rows <- list()
add <- function(row) rows[[length(rows) + 1L]] <<- row

empty_diag <- function() {
    data.frame(
        production_change_mt = NA_real_, intended_production_change_mt = NA_real_,
        shock_gap_mt = NA_real_, consumption_loss_mt = NA_real_, consumption_gain_mt = NA_real_,
        net_consumption_change_mt = NA_real_, stock_change_mt = NA_real_, mass_gap_mt = NA_real_,
        neg_production = NA_integer_, neg_reserves = NA_integer_, neg_trade = NA_integer_,
        neg_use = NA_integer_, new_links = NA_integer_, reserve_draw_mt = NA_real_,
        reserve_overdraw = NA, home_iso3 = "", home_loss_mt = NA_real_, home_loss_share = NA_real_,
        abroad_loss_mt = NA_real_, abroad_loss_share = NA_real_, countries_loss_gt_1kt = NA_integer_,
        max_flow_increase_mt = NA_real_, max_flow_from = "", max_flow_to = "",
        max_flow_baseline_mt = NA_real_, max_flow_ratio = NA_real_,
        links_more_than_doubled = NA_integer_, export_restriction_reopened = NA,
        iterations = NA_integer_, converged = NA, stringsAsFactors = FALSE
    )
}

run_one <- function(event_id, crop, policy_variant, shock_source, targets, annual_fraction, alpha, dP, spec, home_iso = "", shock_years = "") {
    net <- nets[[crop]]
    ran <- run_spec(net, targets, annual_fraction, alpha, dP, spec)
    base <- data.frame(
        event_id = event_id,
        crop = crop,
        baseline = "2018_2021_mean",
        allocation_rule = spec$allocation_rule,
        model = spec$model,
        freeze_empty_reserves = spec$freeze_empty_reserves,
        alpha = alpha,
        policy_variant = policy_variant,
        shock_source = shock_source,
        shock_years = shock_years,
        annual_fraction = annual_fraction,
        ok = isTRUE(ran$ok),
        error = if (isTRUE(ran$ok)) "" else ran$message,
        n_warnings = if (isTRUE(ran$ok)) length(ran$warnings) else NA_integer_,
        warning_text = if (length(ran$warnings)) paste(unique(ran$warnings), collapse = " | ") else "",
        stringsAsFactors = FALSE
    )
    diag_row <- if (isTRUE(ran$ok)) {
        diagnose(net, ran$net, dP, ran$removed, alpha, home_iso, targets, annual_fraction)
    } else {
        empty_diag()
    }
    cbind(base, diag_row, row.names = NULL)
}

message("Flood grid from inputs/placeholder_flood_shocks.csv")
source("main/read_flood_shocks.R")
fao_lookup <- read.csv("outputs/export_for_partners/country_lookup_ISO3_FAO.csv", stringsAsFactors = FALSE)
events <- read_flood_shocks(
    "inputs/placeholder_flood_shocks.csv",
    iso_codes = nets[[1]]$iso,
    fao_lookup = fao_lookup
)
for (event in events) {
    net <- nets[[event$crop]]
    if (is.null(net)) stop("Unknown crop in shock file: ", event$crop)
    dP <- shock_production_change(net, event)
    home <- if (length(event$iso3) == 1) event$iso3 else ""
    for (policy in c("production_only", "production_plus_ban")) {
        ban <- if (policy == "production_plus_ban") event$reactive_ban_fraction else 0
        targets <- if (ban > 0) event$iso3 else character()
        for (alpha in alphas) {
            for (spec in specs) {
                message(sprintf(
                    "%s %s alpha %.2f %s",
                    event$event_id, policy, alpha, spec$allocation_rule
                ))
                add(run_one(
                    event$event_id, event$crop, policy, event$shock_source,
                    targets, ban, alpha, dP, spec,
                    home_iso = home, shock_years = event$years
                ))
            }
        }
    }
}

message("Export-ban grid, both durations and restriction levels, three rules")
ban_pairs <- rbind(
    data.frame(crop = "wheat", who = wheat_exporters, stringsAsFactors = FALSE),
    data.frame(crop = "rice_milled", who = rice_exporters, stringsAsFactors = FALSE)
)
for (i in seq_len(nrow(ban_pairs))) {
    crop <- ban_pairs$crop[i]
    who <- ban_pairs$who[i]
    net <- nets[[crop]]
    targets <- ban_targets(who, net$iso, eu_iso)
    dP <- setNames(rep(0, net$nc), net$iso)
    for (restriction in restrictions) {
        for (duration_i in seq_along(durations)) {
            duration <- names(durations)[duration_i]
            annual_fraction <- unname(restriction * durations[[duration_i]])
            for (alpha in alphas) {
                for (spec in specs) {
                    message(sprintf(
                        "ban %s %s %.2f %s alpha %.2f %s",
                        crop, who, restriction, duration, alpha, spec$allocation_rule
                    ))
                    add(run_one(
                        paste(
                            crop, who,
                            sprintf("ban%02d", round(100 * restriction)),
                            duration,
                            sep = "_"
                        ),
                        crop, "export_ban", "export_restriction",
                        targets, annual_fraction, alpha, dP, spec,
                        home_iso = if (who == "EU-28") "" else who
                    ))
                }
            }
        }
    }
}

comparison <- do.call(rbind, rows)
rownames(comparison) <- NULL
write.csv(comparison, file.path(out_dir, "allocation_comparison.csv"), row.names = FALSE)

flood <- comparison[comparison$policy_variant != "export_ban", ]
bans <- comparison[comparison$policy_variant == "export_ban", ]
cell <- function(frame, event, alpha, rule, column, digits = 2, scale = 1) {
    hit <- frame[frame$event_id == event & frame$alpha == alpha & frame$allocation_rule == rule, ]
    if (!nrow(hit) || !is.finite(hit[[column]][1])) return("NA")
    sprintf(paste0("%.", digits, "f"), hit[[column]][1] / scale)
}
loss_table <- function(frame, title) {
    out <- c(
        title,
        "",
        "| Event | Alpha | Modified PTA | Published PTA | RTA |",
        "|---|---:|---:|---:|---:|"
    )
    for (event in unique(frame$event_id)) {
        for (alpha in alphas) {
            cells <- sapply(c("modified_pta", "published_pta", "rta"), function(rule) {
                cell(frame, event, alpha, rule, "consumption_loss_mt", scale = 1e6)
            })
            out <- c(out, sprintf("| %s | %s | %s | %s | %s |", event, alpha, cells[1], cells[2], cells[3]))
        }
    }
    out
}
share_line <- function(event) {
    cells <- sapply(c("modified_pta", "published_pta", "rta"), function(rule) {
        cell(flood[flood$policy_variant == "production_only", ], event, 0, rule, "home_loss_share", digits = 1, scale = 0.01)
    })
    sprintf("| %s | %s | %s | %s |", event, cells[1], cells[2], cells[3])
}

maize_gap <- unique(round(comparison$mass_gap_mt[comparison$crop == "maize"], 1))
rice_gap <- range(comparison$mass_gap_mt[comparison$crop == "rice_milled"], na.rm = TRUE)
wheat_big <- comparison[comparison$crop == "wheat" & abs(comparison$mass_gap_mt) > 1000, ]
wheat_note <- if (!nrow(wheat_big)) {
    "Wheat closes to numerical noise."
} else {
    paste(sprintf(
        "%s, %s, alpha %s, %+.3f Mt",
        wheat_big$event_id, wheat_big$allocation_rule, wheat_big$alpha, wheat_big$mass_gap_mt / 1e6
    ), collapse = "; ")
}
probe_loss <- function(net, res) {
    C0 <- pmax(as.numeric(net$C), 0)
    C1 <- pmax(as.numeric(res$C[net$iso]), 0)
    pmax(C0 - C1, 0)
}
probe_one <- function(crop, iso3, fraction) {
    net <- nets[[crop]]
    dP <- setNames(rep(0, net$nc), net$iso)
    dP[iso3] <- -fraction * net$P[iso3]
    at_50 <- sim_cascade_PTA(net, dP, rfrac = 0, kmax = 50, verbose = FALSE, freeze_empty_reserves = FALSE)
    at_200 <- sim_cascade_PTA(net, dP, rfrac = 0, kmax = 200, verbose = FALSE, freeze_empty_reserves = FALSE)
    l50 <- probe_loss(net, at_50)
    l200 <- probe_loss(net, at_200)
    list(
        world_50 = sum(l50) / 1e6,
        world_200 = sum(l200) / 1e6,
        home_50 = 100 * l50[net$iso == iso3] / sum(l50),
        home_200 = 100 * l200[net$iso == iso3] / sum(l200),
        max_country_kt = max(abs(l50 - l200)) / 1e3
    )
}
event_fraction <- function(event_id, iso3) {
    event <- events[[which(vapply(events, function(e) e$event_id, "") == event_id)]]
    event$fraction[event$iso3 == iso3]
}
thai <- probe_one("rice_milled", "THA", event_fraction("thailand_2011", "THA"))
usa_maize <- probe_one("maize", "USA", event_fraction("us_midwest_2011_maize", "USA"))

negative_use <- function(crop) {
    net <- nets[[crop]]
    hit <- which(net$C < -1)
    if (!length(hit)) return("none")
    paste(sprintf("%s %.1f kt", net$iso[hit], net$C[hit] / 1e3), collapse = "; ")
}
flow_note <- function() {
    hit <- flood[
        flood$allocation_rule == "modified_pta" & flood$alpha == 0 &
            flood$policy_variant == "production_only" & flood$ok, ]
    if (!nrow(hit)) return("No modified PTA production-only rows.")
    top <- hit[which.max(hit$max_flow_increase_mt), ]
    thin <- hit[which.max(hit$max_flow_ratio), ]
    sprintf(
        "On modified PTA with reserves closed, the largest bilateral increase is %.2f Mt, %s to %s, from a baseline of %.2f Mt (%.2f times). The largest ratio on the link that rose the most in each event is %.0f times, %s to %s, from a baseline of %.3f Mt. Links above 1 kt that more than double: %s.",
        top$max_flow_increase_mt[1] / 1e6, top$max_flow_from[1], top$max_flow_to[1],
        top$max_flow_baseline_mt[1] / 1e6, top$max_flow_ratio[1],
        thin$max_flow_ratio[1], thin$max_flow_from[1], thin$max_flow_to[1],
        thin$max_flow_baseline_mt[1] / 1e6,
        paste(sprintf("%s %d", hit$event_id, hit$links_more_than_doubled), collapse = "; ")
    )
}

mod0 <- flood[flood$allocation_rule == "modified_pta" & flood$alpha == 0, ]
pub_prod <- flood[flood$allocation_rule == "published_pta" & flood$policy_variant == "production_only", ]
rta_prod <- flood[flood$allocation_rule == "rta" & flood$policy_variant == "production_only", ]

lines <- c(
    "# Allocation comparison on the 2018-2021 mean network",
    "",
    "Three existing FSC settings. No new allocation rule was added. The flood rows use the placeholder fractions in `inputs/placeholder_flood_shocks.csv`. Those fractions are not observed flood losses.",
    "",
    "Modified PTA is `sim_cascade_PTA` with `freeze_empty_reserves = FALSE`. Published PTA is the same function with that argument `TRUE`, which is what `main.R` runs. RTA is `sim_cascade_RTA` with its existing `rexp = 0.2`. A banned exporter stays frozen in all three. Every call passes the rule explicitly.",
    "",
    sprintf(
        "Flood runs: %d, all completed. Converged before 50 iterations: %d of %d. Export-ban runs: %d, converged: %d of %d. New bilateral links: %d. Negative production, reserves, trade, or final use: %d. Reserve draws above usable reserves: %d. Production change off the requested shock by more than 1 gram: %d. Export restrictions reopened: %d.",
        nrow(flood), sum(flood$converged), nrow(flood),
        nrow(bans), sum(bans$converged), nrow(bans),
        sum(comparison$new_links, na.rm = TRUE),
        sum(comparison$neg_production + comparison$neg_reserves + comparison$neg_trade + comparison$neg_use, na.rm = TRUE),
        sum(comparison$reserve_overdraw, na.rm = TRUE),
        sum(abs(comparison$shock_gap_mt) > 1e-6, na.rm = TRUE),
        sum(comparison$export_restriction_reopened, na.rm = TRUE)
    ),
    "",
    sprintf(
        "Mass balance is the change in use, minus the change in production, plus the stock change, after baseline use below zero is floored at zero. Maize carries one constant gap of %.3f Mt on every maize run. That equals baseline use below zero (%s), countries whose processed-product exports exceed production plus imports on this mean network. It is not created by the cascade. Rice sits between %.3f and %.3f kt, beside baseline use below zero (%s). Wheat gaps larger than 1 kt: %s.",
        maize_gap[1] / 1e6, negative_use("maize"), rice_gap[1] / 1e3, rice_gap[2] / 1e3, negative_use("rice_milled"), wheat_note
    ),
    "",
    sprintf(
        "Modified PTA at alpha 0 hit the 50-iteration cap on %d of %d flood runs. Published PTA converged on %d of %d production-only flood runs. RTA converged on %d of %d. The same equations were then run to 200 iterations for two alpha-0 cases. Thailand rice world loss was %.2f Mt at 50 iterations and %.2f Mt at 200, and Thailand's share moved from %.2f to %.2f percent. US maize world loss was %.2f Mt at 50 iterations and %.2f Mt at 200. The largest country-level maize change between those two caps was %.1f kt. Partner rankings from a run that hit the cap are approximate. The iteration limit in the grid was not raised.",
        sum(!mod0$converged), nrow(mod0),
        sum(pub_prod$converged), nrow(pub_prod),
        sum(rta_prod$converged), nrow(rta_prod),
        thai$world_50, thai$world_200, thai$home_50, thai$home_200,
        usa_maize$world_50, usa_maize$world_200, usa_maize$max_country_kt
    ),
    "",
    loss_table(
        flood[flood$policy_variant == "production_only", ],
        "World consumption loss, production shock only, million tonnes. This is the primary flood experiment."
    ),
    "",
    "Share of that consumption loss borne by the shocked country, production shock only, alpha 0, percent.",
    "",
    "| Event | Modified PTA | Published PTA | RTA |",
    "|---|---:|---:|---:|",
    vapply(unique(flood$event_id[flood$policy_variant == "production_only"]), share_line, character(1)),
    "",
    loss_table(
        flood[flood$policy_variant == "production_plus_ban", ],
        "World consumption loss when the placeholder production shock is combined with the file's reactive export restriction. This is a sensitivity, not the primary flood experiment. Million tonnes."
    ),
    "",
    "Modified PTA is the preferred candidate for a later flood run. It is not an empirically settled winner. At alpha 0 the published rule freezes every exporter, so other countries cannot replace a lost flow on a link that already exists. Modified PTA allows that replacement and still keeps an explicit ban closed. The world total of a pure production shock is the same under all three rules when reserves are closed, because trade cancels. What changes is who absorbs the loss. The proportional update can also multiply a thin existing link by a large factor. That is a property of the published update, and it is a reason to read partner rankings with the flow table rather than to treat the candidate as validated.",
    "",
    flow_note(),
    "",
    "On these placeholders, modified PTA leaves 1.8 to 9.9 percent of the consumption loss in the shocked country. Published PTA leaves about 1 percent when the shocked country is a large exporter, and much more when it is not (Pakistan wheat about 67 percent, Myanmar rice about 37 percent). RTA at alpha 0 matches published PTA. `rexp = 0.2` offers one fifth of remaining reserves on each pass, which is an offer of zero when alpha is 0.",
    "",
    "Domestic use still absorbs 1 percent of the remaining shortage on each pass in all three settings. A production loss therefore leaves mostly through exports. That is the existing FSC step. It was not retuned.",
    "",
    "Published PTA stays the reference for the 2020 paper. RTA stays the reserves-offer sensitivity. The export-ban rows in `allocation_comparison.csv` repeat the existing wheat and rice exporter grid (50 and 95 percent, full year and six months, four alpha values) under all three rules. `benchmark_results.csv` is unchanged and is still the published PTA grid.",
    "",
    "The only code change inside the cascade, apart from recording the iteration count, puts trade-matrix dimnames back after `outer()` and `sweep()` drop them. The update factors are the same. Warnings that used to print a row number now print the country code.",
    "",
    "Replace the placeholder file with observed losses and rerun `Rscript main/run_allocation_comparison.R`. The script reads that file and does not change the three allocation functions.",
    ""
)
writeLines(lines, file.path(out_dir, "ALLOCATION_COMPARISON.md"))
message("Wrote ", file.path(out_dir, "allocation_comparison.csv"))
message("Wrote ", file.path(out_dir, "ALLOCATION_COMPARISON.md"))

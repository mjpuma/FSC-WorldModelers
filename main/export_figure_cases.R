# Country-level outcomes for the report figures.
#   FSC_SOURCE_ONLY=1 Rscript main/export_figure_cases.R
# Run from the repository root.

Sys.setenv(FSC_SOURCE_ONLY = "1")
source("main/run_flood_benchmarks.R")

out_dir <- "outputs/flood_benchmarks/figures"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cases <- list(
    list(id = "wheat_RUS", crop = "wheat", who = "RUS"),
    list(id = "wheat_EU28", crop = "wheat", who = "EU-28"),
    list(id = "wheat_USA", crop = "wheat", who = "USA"),
    list(id = "rice_IND", crop = "rice_milled", who = "IND"),
    list(id = "rice_THA", crop = "rice_milled", who = "THA"),
    list(id = "rice_VNM", crop = "rice_milled", who = "VNM"),
    list(id = "rice_PAK", crop = "rice_milled", who = "PAK"),
    list(id = "maize_USA", crop = "maize", who = "USA")
)

countries <- list()
edges <- list()
diagnostics <- list()
metrics <- list()

save_case <- function(case_id, crop, who, alpha, fraction, dP = NULL,
                      rule = "frozen", model = "PTA") {
    net <- nets[[crop]]
    targets <- ban_targets(who, net$iso, eu_iso)
    ran <- run_case(
        net, targets, fraction, alpha, dP = dP, model = model,
        freeze_empty_reserves = identical(rule, "frozen")
    )
    if (!isTRUE(ran$ok)) {
        diagnostics[[length(diagnostics) + 1L]] <<- data.frame(
            case_id = case_id, rule = rule, model = model, alpha = alpha,
            ok = FALSE, error = ran$message,
            stringsAsFactors = FALSE
        )
        return(invisible(NULL))
    }
    final <- ran$net
    C0 <- pmax(net$C, 0)
    C1 <- pmax(as.numeric(final$C[net$iso]), 0)
    names(C1) <- net$iso
    loss <- C0 - C1
    countries[[length(countries) + 1L]] <<- data.frame(
        case_id = case_id, crop = crop, shocked = who, rule = rule,
        model = model, alpha = alpha, iso3 = net$iso,
        consumption_t0_mt = as.numeric(C0),
        consumption_t1_mt = as.numeric(C1),
        consumption_loss_mt = as.numeric(loss),
        loss_fraction = ifelse(C0 > 0, as.numeric(loss) / as.numeric(C0), NA_real_),
        stringsAsFactors = FALSE
    )
    E0 <- net$E
    E1 <- final$E
    dimnames(E1) <- dimnames(E0)
    delta <- E0 - E1
    keep <- which(E0 >= 1000 | abs(delta) >= 1000, arr.ind = TRUE)
    if (nrow(keep)) {
        edges[[length(edges) + 1L]] <<- data.frame(
            case_id = case_id, rule = rule, model = model, alpha = alpha,
            exporter_iso3 = rownames(E0)[keep[, 1]],
            importer_iso3 = colnames(E0)[keep[, 2]],
            trade_t0_mt = E0[keep],
            trade_t1_mt = E1[keep],
            export_cut_mt = delta[keep],
            stringsAsFactors = FALSE
        )
    }
    link0 <- E0 >= 1000
    link1 <- E1 >= 1000
    diag(link0) <- FALSE
    diag(link1) <- FALSE
    metrics[[length(metrics) + 1L]] <<- data.frame(
        case_id = case_id, rule = rule, model = model, alpha = alpha,
        countries = nrow(E0),
        links_t0 = sum(link0),
        links_t1 = sum(link1),
        mean_out_degree_t0 = mean(rowSums(link0)),
        mean_out_degree_t1 = mean(rowSums(link1)),
        trade_t0_mt = sum(E0),
        trade_t1_mt = sum(E1),
        stringsAsFactors = FALSE
    )
    world_loss <- sum(pmax(loss, 0), na.rm = TRUE)
    world_gain <- sum(pmax(-loss, 0), na.rm = TRUE)
    production_change <- sum(final$P - net$P)
    stock_change <- sum(final$dR)
    diagnostics[[length(diagnostics) + 1L]] <<- data.frame(
        case_id = case_id, rule = rule, model = model, alpha = alpha,
        ok = TRUE, error = "",
        consumption_loss_mt = world_loss,
        consumption_gain_mt = world_gain,
        net_consumption_change_mt = sum(C1 - C0),
        production_change_mt = production_change,
        stock_change_mt = stock_change,
        identity_gap_mt = sum(C1 - C0) - production_change + stock_change,
        stringsAsFactors = FALSE
    )
}

for (case in cases) {
    for (alpha in c(0, 1)) {
        message(case$id, " frozen alpha ", alpha)
        save_case(case$id, case$crop, case$who, alpha, 0.95, rule = "frozen", model = "PTA")
    }
    message(case$id, " reroute alpha 0")
    save_case(case$id, case$crop, case$who, 0, 0.95, rule = "reroute", model = "PTA")
}

for (case in cases[c(1, 4, 8)]) {
    for (alpha in c(0, 1)) {
        message(case$id, " RTA alpha ", alpha)
        save_case(case$id, case$crop, case$who, alpha, 0.95, rule = "rta", model = "RTA")
    }
}

shocks <- read.csv("inputs/placeholder_flood_shocks.csv", stringsAsFactors = FALSE)
for (i in seq_len(nrow(shocks))) {
    shock <- shocks[i, ]
    for (alpha in c(0, 1)) {
        for (variant in c("production_only", "production_plus_ban")) {
            ban <- if (variant == "production_plus_ban") shock$reactive_ban_fraction else 0
            dP <- setNames(rep(0, nets[[shock$crop]]$nc), nets[[shock$crop]]$iso)
            dP[shock$iso3] <- -shock$production_loss_fraction * nets[[shock$crop]]$P[shock$iso3]
            case_id <- paste(shock$event_id, variant, sep = "__")
            message(case_id, " frozen alpha ", alpha)
            save_case(case_id, shock$crop, shock$iso3, alpha, ban, dP = dP,
                      rule = "frozen", model = "PTA")
            if (alpha == 0) {
                message(case_id, " reroute alpha 0")
                save_case(case_id, shock$crop, shock$iso3, alpha, ban, dP = dP,
                          rule = "reroute", model = "PTA")
            }
        }
    }
}

write.csv(do.call(rbind, countries), file.path(out_dir, "country_outcomes.csv"), row.names = FALSE)
write.csv(do.call(rbind, edges), file.path(out_dir, "edge_outcomes.csv"), row.names = FALSE)
write.csv(do.call(rbind, diagnostics), file.path(out_dir, "mass_balance.csv"), row.names = FALSE)
write.csv(do.call(rbind, metrics), file.path(out_dir, "network_metrics.csv"), row.names = FALSE)
message("Wrote figure inputs")

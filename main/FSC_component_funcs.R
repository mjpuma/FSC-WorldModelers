## Functions for the Food Shocks Cascade (FSC) model

# Calculate net supply (P + I - E - dR) for each country in a food_net object
get_supply <- function(food_net) {
    food_net$P + colSums(food_net$E) - rowSums(food_net$E) - food_net$dR
}

# Update shortage values in food_net object.
# Tolerance is relative so the same check works in kcal and in metric tons.
update_shortage <- function(food_net) {
    food_net$shortage <- food_net$C - get_supply(food_net)
    scale <- pmax(abs(food_net$C), abs(food_net$P), 1)
    if (any(food_net$shortage < -1e-8 * scale, na.rm = TRUE)) {
        stop("Negative shortage.")
    }
    food_net
}

# Signed shock fractions use the scenario-file convention:
# positive = production decline, negative = production gain.
# Both sides are clamped to a magnitude of 1.
clamp_shock_fraction <- function(fraction, iso3 = NULL, year_label = "shock") {
    f <- suppressWarnings(as.numeric(fraction))
    bad <- !is.finite(f)
    if (any(bad)) {
        warning(sprintf("%s: %d non-finite shock fractions set to 0", year_label, sum(bad)),
                call. = FALSE)
        f[bad] <- 0
    }
    hi <- which(f > 1)
    lo <- which(f < -1)
    if (length(hi)) {
        lab <- if (!is.null(iso3)) paste(iso3[hi], collapse = ", ") else paste(hi, collapse = ", ")
        warning(sprintf("%s: decline fraction > 1 clamped to 1 for: %s", year_label, lab),
                call. = FALSE)
        f[hi] <- 1
    }
    if (length(lo)) {
        lab <- if (!is.null(iso3)) paste(iso3[lo], collapse = ", ") else paste(lo, collapse = ", ")
        warning(sprintf("%s: gain fraction < -1 clamped to -1 for: %s", year_label, lab),
                call. = FALSE)
        f[lo] <- -1
    }
    f
}

clamp_alpha <- function(rfrac) {
    r <- suppressWarnings(as.numeric(rfrac)[1])
    if (length(r) != 1 || !is.finite(r)) {
        stop("fractional reserve access (alpha) must be a single finite number")
    }
    if (r < 0 || r > 1) {
        r_clamped <- min(1, max(0, r))
        warning(sprintf("alpha %s is outside [0, 1]; clamped to %s", r, r_clamped),
                call. = FALSE)
        r <- r_clamped
    }
    r
}

safe_ratio <- function(num, den, eps = 1e-8) {
    out <- rep(NA_real_, length(num))
    ok <- is.finite(den) & abs(den) > eps & is.finite(num)
    out[ok] <- num[ok] / den[ok]
    out
}

assert_no_self_loops <- function(E, tol = 0) {
    d <- diag(E)
    n_bad <- sum(abs(d) > tol, na.rm = TRUE)
    if (n_bad > 0) {
        stop(sprintf(
            "Trade matrix has %d nonzero diagonal entries (sum = %g). Self-loops must be zero.",
            n_bad, sum(d, na.rm = TRUE)
        ))
    }
    invisible(TRUE)
}

# fraction_by_iso is the share of each country's exports that is banned, in [0, 1].
# The banned volume is added to that country's reserves and removed from its export row.
apply_export_bans <- function(E, reserves, fraction_by_iso) {
    if (is.null(names(fraction_by_iso))) {
        stop("fraction_by_iso must be named by iso3")
    }
    if (is.null(rownames(E))) {
        stop("Export matrix needs iso3 rownames")
    }
    iso_names <- names(fraction_by_iso)
    frac_vals <- as.numeric(fraction_by_iso)
    frac_vals[!is.finite(frac_vals)] <- 0
    if (any(frac_vals < 0 | frac_vals > 1)) {
        warning("Export-ban fractions outside [0, 1] were clamped", call. = FALSE)
        frac_vals <- pmin(pmax(frac_vals, 0), 1)
    }
    names(frac_vals) <- iso_names
    unknown <- setdiff(iso_names[frac_vals > 0], rownames(E))
    if (length(unknown)) {
        warning("Export-ban iso3 not in the matrix: ", paste(unknown, collapse = ", "),
                call. = FALSE)
    }
    if (length(reserves) != nrow(E)) {
        stop("reserves length does not match the export matrix")
    }
    frac <- frac_vals[rownames(E)]
    frac[is.na(frac)] <- 0
    removed <- frac * rowSums(E)
    reserves <- as.numeric(reserves) + removed
    E <- sweep(E, 1, 1 - frac, `*`)
    list(E = E, reserves = reserves, removed = removed)
}

# Absorb any shortage less than a fraction eps of positive consumption.
absorb_tiny_shortages <- function(food_net, eps = 1E-5) {
    domestic <- pmax(food_net$C, 0)
    tiny <- food_net$shortage > 0 & food_net$shortage <= eps * domestic & domestic > 0
    absorb_amt <- ifelse(tiny, pmin(food_net$shortage, domestic), 0)
    food_net$C <- food_net$C - absorb_amt
    update_shortage(food_net)
}

# Draw from reserves before residual deficits are split between consumption and exports.
# Draws are bounded by usable reserves and cannot increase stocks via a negative need.
draw_reserves <- function(food_net, need = food_net$shortage, avail = food_net$R) {
    need <- pmax(as.numeric(need), 0)
    avail <- pmax(as.numeric(avail), 0)
    need[!is.finite(need)] <- 0
    avail[!is.finite(avail)] <- 0
    drawn <- pmin(need, avail)
    food_net$dR <- food_net$dR - drawn
    food_net$R <- pmax(food_net$R - drawn, 0)
    update_shortage(food_net)
}

# Stock-then-split step. After reserves have been drawn, each country whose
# shortage is at least `tol` tonnes splits that remainder between an export cut
# and a domestic-use cut in proportion to current exports and domestic use.
# The export cut is taken pro rata from links that already exist. No new link
# is opened. This is not the published PTA or RTA update.
split_residual_proportional <- function(food_net, tol = 1) {
    labels <- rownames(food_net$E)
    shortage <- pmax(as.numeric(food_net$shortage), 0)
    shortage[!is.finite(shortage)] <- 0
    exports <- pmax(rowSums(food_net$E), 0)
    use <- pmax(as.numeric(food_net$C), 0)
    denom <- exports + use
    active <- shortage >= tol & denom > 0
    exp_cut <- rep(0, length(shortage))
    use_cut <- rep(0, length(shortage))
    exp_cut[active] <- shortage[active] * exports[active] / denom[active]
    use_cut[active] <- shortage[active] * use[active] / denom[active]
    exp_cut <- pmin(exp_cut, exports)
    use_cut <- pmin(use_cut, use)
    scale <- ifelse(exports > 0, 1 - exp_cut / exports, 1)
    food_net$E <- sweep(food_net$E, 1, scale, `*`)
    food_net$E[food_net$E < 0] <- 0
    if (!is.null(labels)) dimnames(food_net$E) <- list(labels, labels)
    # use_cut is already bounded by positive domestic use, so this does not lift
    # a negative baseline use up to zero. That baseline is a processed-trade
    # imbalance, not a shortage the flood created.
    food_net$C <- food_net$C - use_cut
    food_net$last_split_tonnes <- sum(exp_cut) + sum(use_cut)
    update_shortage(food_net)
}

# Cut consumption by a requested amount, never below zero and never by more than
# current domestic use. A non-positive request removes nothing.
cut_consumption <- function(food_net, amount = food_net$shortage) {
    amount <- pmax(as.numeric(amount), 0)
    amount[!is.finite(amount)] <- 0
    amount <- pmin(amount, pmax(food_net$C, 0))
    food_net$C <- food_net$C - amount
    update_shortage(food_net)
}

# Reallocate trade for countries based on additional supply needed (need).
# Countries with need decrease exports and increase imports by the same proportion
# across existing trade links. Export reductions stay inside current exports.
realloc_trade_prop_link <- function(food_net, need = food_net$shortage,
                                    exp_freeze = food_net$R == 0) {
    need <- pmax(as.numeric(need), 0)
    need[!is.finite(need)] <- 0
    Tavail <- rowSums(food_net$E) + colSums(food_net$E[!exp_freeze, , drop = FALSE])
    prop_dT <- ifelse(Tavail == 0, 0, pmin(need / Tavail, 1))
    prop_dT[!is.finite(prop_dT)] <- 0

    prop_dE <- outer(!exp_freeze, prop_dT) %>%
        sweep(1, prop_dT, `-`)

    labels <- rownames(food_net$E)
    food_net$E <- (1 + prop_dE) * food_net$E
    food_net$E[food_net$E < 0] <- 0
    # outer() drops dimnames. Put them back. The factors above are unchanged.
    if (!is.null(labels)) dimnames(food_net$E) <- list(labels, labels)
    update_shortage(food_net)
}

# Reduce exports for countries with need. The cut on each row is at most that row's exports.
reduce_exp_prop_link <- function(food_net, need = food_net$shortage) {
    need <- pmax(as.numeric(need), 0)
    need[!is.finite(need)] <- 0
    Esum <- rowSums(food_net$E)
    prop_dE <- ifelse(Esum == 0, 0, pmin(need / Esum, 1))
    prop_dE[!is.finite(prop_dE)] <- 0
    labels <- rownames(food_net$E)
    food_net$E <- sweep(food_net$E, 1, 1 - prop_dE, `*`)
    food_net$E[food_net$E < 0] <- 0
    if (!is.null(labels)) dimnames(food_net$E) <- list(labels, labels)
    update_shortage(food_net)
}

# Increase imports based on importer need and exporter reserves.
increase_imp_prop_resv <- function(food_net, need = food_net$shortage,
                                   avail = food_net$R) {
    need <- pmax(as.numeric(need), 0)
    avail <- pmax(as.numeric(avail), 0)
    need[!is.finite(need)] <- 0
    avail[!is.finite(avail)] <- 0
    dE <- matrix(0, nrow = food_net$nc, ncol = food_net$nc)
    guard <- 0
    while (any(need > 0) && guard < 10000) {
        guard <- guard + 1
        E_w_need <- sweep(food_net$E, 2, need > 0, `*`)
        denom <- rowSums(E_w_need)
        prop_dE <- ifelse(denom == 0, 0, avail / denom)
        prop_dE[!is.finite(prop_dE)] <- 0
        E_offer <- sweep(E_w_need, 1, prop_dE, `*`)
        offer_tot <- colSums(E_offer)
        prop_dI <- ifelse(offer_tot == 0, 0, pmin(need / offer_tot, 1))
        prop_dI[!is.finite(prop_dI)] <- 0
        E_accept <- sweep(E_offer, 2, prop_dI, `*`)
        if (sum(E_accept) < 1) break
        need <- pmax(need - colSums(E_accept), 0)
        avail <- pmax(avail - rowSums(E_accept), 0)
        dE <- dE + E_accept
    }
    labels <- rownames(food_net$E)
    food_net$E <- food_net$E + dE
    food_net$E[food_net$E < 0] <- 0
    if (!is.null(labels)) dimnames(food_net$E) <- list(labels, labels)
    update_shortage(food_net)
}

# If exports still exceed available supply, cut them pro-rata so domestic use is
# not negative. The cut cannot exceed the export row.
enforce_nonnegative_domestic <- function(food_net) {
    supply <- get_supply(food_net)
    excess_exports <- pmax(0, -supply)
    Esum <- pmax(rowSums(food_net$E), 0)
    cut <- pmin(excess_exports, Esum)
    prop <- ifelse(Esum <= 0, 0, cut / Esum)
    prop[!is.finite(prop)] <- 0
    if (any(prop > 0)) {
        who <- if (!is.null(rownames(food_net$E))) {
            rownames(food_net$E)[prop > 0]
        } else {
            which(prop > 0)
        }
        warning("Reduced exports to keep domestic use non-negative: ",
                paste(who, collapse = ", "), call. = FALSE)
        food_net$E <- sweep(food_net$E, 1, 1 - prop, `*`)
    }
    food_net$P <- pmax(food_net$P, 0)
    food_net$R <- pmax(food_net$R, 0)
    food_net$C <- pmax(get_supply(food_net), 0)
    update_shortage(food_net)
}

# Diagnostic: output of sim_cascade should respect S = P + I - E = R + C.
sim_diagnostics <- function(sim_res, tol = 1E-5) {
    S0 <- sim_res$P0 + colSums(sim_res$E0) - rowSums(sim_res$E0)
    dS0 <- sum(sim_res$dP)
    cnames <- names(sim_res$P0)

    if (!is.finite(dS0) || abs(dS0) <= tol) {
        return("No aggregate production shock; skipped the global identity ratio.")
    }

    discrep <- (sum(sim_res$dR) + sum(sim_res$dC)) / dS0 - 1
    if (!is.finite(discrep) || abs(discrep) > tol) {
        return(paste("Total dC + dR does not match initial shock.",
                     "Relative difference:", discrep))
    }

    dS_byc <- sim_res$dP + colSums(sim_res$dE) - rowSums(sim_res$dE)
    den <- dS_byc + S0
    rel <- rep(0, length(den))
    ok <- is.finite(den) & abs(den) > tol
    rel[ok] <- (dS_byc[ok] - sim_res$dR[ok] - sim_res$dC[ok]) / den[ok]
    discrep_i <- which(abs(rel) > tol)
    if (length(discrep_i) > 0) {
        return(paste("Net supply change does not match dC + dR for countries:",
                     paste(cnames[discrep_i], collapse = ", ")))
    }

    "All tests passed."
}

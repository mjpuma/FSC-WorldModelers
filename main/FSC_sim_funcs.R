# Main simulation loop to implement Food Shock Cascade (FSC) Model ----

# Apply a production shock and open usable reserves before the cascade.
# dP must be zero or negative. Losses larger than baseline production are clamped
# so production cannot become negative. The initial shortage is the supply gap
# with trade held fixed, which is what reserve drawdown sees first.
prepare_shocked_network <- function(food_net, dP, rfrac) {
    assert_no_self_loops(food_net$E)
    rfrac <- clamp_alpha(rfrac)
    dP_names <- names(dP)
    dP <- as.numeric(dP)
    if (length(dP) != length(food_net$P)) {
        stop("dP length does not match the production vector")
    }
    labels <- names(food_net$P)
    if (!is.null(dP_names) && !is.null(labels)) {
        if (!setequal(dP_names, labels)) {
            stop("dP names do not match the production vector")
        }
        dP <- dP[match(labels, dP_names)]
    }
    dP[!is.finite(dP)] <- 0
    if (is.null(labels)) labels <- if (!is.null(dP_names)) dP_names else as.character(seq_along(dP))
    names(dP) <- labels

    gains <- which(dP > 0)
    if (length(gains)) {
        warning("Positive dP was set to 0 (book production gains as reserves) for: ",
                paste(labels[gains], collapse = ", "), call. = FALSE)
        dP[gains] <- 0
    }
    over <- which(dP < -food_net$P)
    if (length(over)) {
        warning("Shock exceeds baseline production and was clamped for: ",
                paste(labels[over], collapse = ", "), call. = FALSE)
        dP[over] <- -food_net$P[over]
    }

    food_net$P <- pmax(food_net$P + dP, 0)
    food_net$R[!is.finite(food_net$R)] <- 0
    food_net$R <- rfrac * pmax(food_net$R, 0)
    food_net$dR[!is.finite(food_net$dR)] <- 0
    # Balance gap after the production loss, before export or consumption cuts.
    food_net$shortage <- pmax(0, food_net$C - get_supply(food_net))
    food_net$dP_applied <- dP
    food_net
}

## Simulation with the Proportional Trade Allocation (PTA) version ====
#  dP: vector of initial change in production (zero or negative) by country
#  rfrac: fraction of actual reserves to use (alpha), clamped to [0, 1]
#  cfrac: fraction of residual shortage absorbed by domestic use before trade
#  kmax: maximum number of iterations

sim_cascade_PTA <-
  function(food_net,
           dP,
           rfrac,
           cfrac = 0.01,
           kmax = 50,
           verbose = TRUE,
           trade_freeze = NULL,
           freeze_empty_reserves = TRUE) {
    food_net <- prepare_shocked_network(food_net, dP, rfrac)
    # trade_freeze keeps a policy export cut from being reopened by reallocation.
    # By default, countries with zero usable reserves stay frozen as well.
    # Set freeze_empty_reserves = FALSE to hold stocks unused while still
    # letting other exporters reroute along existing links.
    if (!is.null(trade_freeze)) {
        flag <- as.logical(trade_freeze)
        if (!is.null(names(flag))) {
            flag <- flag[names(food_net$P)]
            flag[is.na(flag)] <- FALSE
        }
        if (length(flag) != length(food_net$P)) {
            stop("trade_freeze must match the production vector")
        }
        food_net$trade_freeze <- flag
    }

    k <- 0L
    converged <- FALSE
    for (k in seq_len(kmax)) {
      if (verbose) cat("iteration", k, "of", kmax, "\n")
      new_food_net <- food_net %>%
        draw_reserves() %>%
        cut_consumption(., amount = cfrac * .$shortage) %>%
        absorb_tiny_shortages()
      freeze <- if (isTRUE(freeze_empty_reserves)) {
          new_food_net$R == 0
      } else {
          rep(FALSE, length(new_food_net$R))
      }
      if (!is.null(new_food_net$trade_freeze)) {
          freeze <- freeze | as.logical(new_food_net$trade_freeze)
      }
      new_food_net <- realloc_trade_prop_link(new_food_net, exp_freeze = freeze)
      if (isTRUE(all.equal.numeric(food_net$E, new_food_net$E, check.attributes = TRUE))) {
        food_net <- new_food_net
        converged <- TRUE
        break
      } else if (k == kmax) {
        warning("No equilibrium reached after maximum number of iterations", call. = FALSE)
      }
      food_net <- new_food_net
    }
    # Diagnostic only. The trade update above is unchanged.
    food_net$cascade_iterations <- as.integer(k)
    food_net$cascade_converged <- converged
    enforce_nonnegative_domestic(food_net)
  }

## Simulation with the Reserves-based Trade Allocation version ====
#  Reserves are drawn before export cuts. Exporters then offer a share of
#  remaining usable reserves.

sim_cascade_RTA <-
  function(food_net,
           dP,
           rfrac,
           rexp = 0.2,
           cfrac = 0.01,
           kmax = 50,
           verbose = TRUE,
           trade_freeze = NULL) {
    food_net <- prepare_shocked_network(food_net, dP, rfrac)
    rexp <- min(1, max(0, as.numeric(rexp)[1]))
    if (!is.null(trade_freeze)) {
        flag <- as.logical(trade_freeze)
        if (!is.null(names(flag))) {
            flag <- flag[names(food_net$P)]
            flag[is.na(flag)] <- FALSE
        }
        if (length(flag) != length(food_net$P)) {
            stop("trade_freeze must match the production vector")
        }
        food_net$trade_freeze <- flag
    }

    k <- 0L
    converged <- FALSE
    for (k in seq_len(kmax)) {
      if (verbose) cat("iteration", k, "of", kmax, "\n")
      new_food_net <- food_net %>%
        draw_reserves() %>%
        cut_consumption(., amount = cfrac * .$shortage) %>%
        absorb_tiny_shortages() %>%
        reduce_exp_prop_link() %>%
        draw_reserves() %>%
        cut_consumption(., amount = cfrac * .$shortage) %>%
        absorb_tiny_shortages()
      offer <- rexp * pmax(new_food_net$R, 0)
      if (!is.null(new_food_net$trade_freeze)) {
          offer[as.logical(new_food_net$trade_freeze)] <- 0
      }
      new_food_net <- increase_imp_prop_resv(new_food_net, avail = offer)
      if (isTRUE(all.equal(food_net$E, new_food_net$E))) {
        food_net <- new_food_net
        converged <- TRUE
        break
      } else if (k == kmax) {
        warning("No equilibrium reached after maximum number of iterations", call. = FALSE)
      }
      food_net <- new_food_net
    }
    # Diagnostic only. The trade update above is unchanged.
    food_net$cascade_iterations <- as.integer(k)
    food_net$cascade_converged <- converged
    enforce_nonnegative_domestic(food_net)
  }

## Stock-then-split cascade ====
# Draw usable reserves first. Split any remaining shortage of at least `tol`
# tonnes between exports and domestic use in proportion to the two, pass the
# export cut to existing partners, and repeat. This does not change PTA or RTA.
# Published main.R does not call it.

sim_cascade_split <-
  function(food_net,
           dP,
           rfrac,
           tol = 1,
           kmax = 5000,
           verbose = FALSE) {
    food_net <- prepare_shocked_network(food_net, dP, rfrac)
    k <- 0L
    converged <- FALSE
    repeat {
      k <- k + 1L
      if (k > kmax) {
        warning("No equilibrium reached after maximum number of iterations", call. = FALSE)
        break
      }
      if (verbose) cat("iteration", k, "of", kmax, "\n")
      if (max(pmax(food_net$shortage, 0), na.rm = TRUE) < tol) {
        converged <- TRUE
        break
      }
      food_net <- draw_reserves(food_net)
      before_E <- food_net$E
      before_C <- food_net$C
      food_net <- split_residual_proportional(food_net, tol = tol)
      stuck <- isTRUE(all.equal(before_E, food_net$E)) &&
        isTRUE(all.equal(before_C, food_net$C))
      if (max(pmax(food_net$shortage, 0), na.rm = TRUE) < tol) {
        converged <- TRUE
        break
      }
      if (stuck || isTRUE(food_net$last_split_tonnes < tol)) {
        warning("Stock-then-split stopped with a remaining shortage above 1 tonne", call. = FALSE)
        break
      }
    }
    food_net$cascade_iterations <- as.integer(k)
    food_net$cascade_converged <- converged
    # Do not call enforce_nonnegative_domestic here. That closer rewrites every
    # country's use to its supply and cuts exports wherever baseline use is
    # already negative. The split leaves those baseline imbalances in place.
    food_net
  }

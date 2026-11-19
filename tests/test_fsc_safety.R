## Bounds and edge cases for the cascade. Run from the repository root:
##   Rscript tests/test_fsc_safety.R

library(dplyr, warn.conflicts = FALSE)
source("main/FSC_component_funcs.R")
source("main/FSC_sim_funcs.R")

failures <- character()
check <- function(cond, label) {
    if (!isTRUE(cond)) {
        failures <<- c(failures, label)
        message("FAIL: ", label)
    } else {
        message("OK: ", label)
    }
}

iso <- c("AAA", "BBB", "CCC")
E <- matrix(c(
    0, 40, 0,
    0,  0, 80,
    0,  0,  0
), 3, byrow = TRUE, dimnames = list(iso, iso))
P <- c(AAA = 100, BBB = 10, CCC = 50)
R <- c(AAA = 50, BBB = 0, CCC = 5)
base <- list(
    P = P, R = R, E = E,
    dR = c(AAA = 0, BBB = 0, CCC = 0),
    C = P + colSums(E) - rowSums(E),
    nc = 3,
    shortage = c(AAA = 0, BBB = 0, CCC = 0)
)
# BBB exports 80 and only receives 40, so baseline domestic use is negative.
check(base$C["BBB"] < 0, "fixture has a re-export deficit")

# Shock larger than production is clamped, and reserves are drawn against the
# resulting balance gap (100), not the unclamped 150.
over <- sim_cascade_PTA(base, dP = c(AAA = -150, BBB = 0, CCC = 0), rfrac = 1,
                         verbose = FALSE)
check(over$P["AAA"] == 0, "production is not negative when the shock exceeds it")
check(over$dP_applied["AAA"] == -100, "applied shock is clamped to baseline production")
check(all(over$C >= -1e-6), "domestic use is non-negative after the cascade")
check(all(over$E >= -1e-8), "exports are non-negative")
check(all(abs(diag(over$E)) == 0), "diagonal stays zero")
check(over$R["AAA"] >= -1e-8, "usable reserves are not negative")
# 50 of the 100-unit gap is covered by reserves; the rest cannot be exported
# (AAA still ships 40 to BBB) so consumption falls, but not below zero.
check(abs(over$dR["AAA"] - (-50)) < 1e-6, "reserve draw equals usable stocks, not the unclamped shock")

# A loss fully covered by reserves leaves consumption unchanged.
covered <- sim_cascade_PTA(base, dP = c(AAA = -20, BBB = 0, CCC = 0), rfrac = 1,
                           verbose = FALSE)
check(abs(covered$P["AAA"] - 80) < 1e-6, "20 percent shock reduces production by 20")
check(abs(covered$C["AAA"] - base$C["AAA"]) < 1e-4, "reserves cover a small shock before consumption is cut")
check(abs(covered$dR["AAA"] - (-20)) < 1e-4, "reserve draw equals the balance gap when stocks suffice")

# Zero-production country: a fractional shock has nothing to remove.
zero_p <- base
zero_p$P["BBB"] <- 0
zero_p$C <- zero_p$P + colSums(zero_p$E) - rowSums(zero_p$E)
zero_run <- sim_cascade_PTA(zero_p, dP = c(AAA = 0, BBB = -5, CCC = 0), rfrac = 0.5,
                            verbose = FALSE)
check(zero_run$P["BBB"] == 0, "a shock on zero production stays at zero")
check(all(is.finite(zero_run$C)), "zero-production shock does not create NaN consumption")

# Alpha outside [0, 1] is clamped. A value of 2 must not draw more than the stock.
alpha_notes <- capture.output(
    alpha_run <- sim_cascade_PTA(base, dP = c(AAA = -100, BBB = 0, CCC = 0), rfrac = 2,
                                 verbose = FALSE),
    type = "message"
)
check(abs(alpha_run$dR["AAA"] - (-50)) < 1e-4, "alpha above 1 does not draw more than physical stocks")

# Direct domestic-cut bound. Supply is zero, so cutting use does not create a surplus.
cut_net <- base
cut_net$E[] <- 0
cut_net$P[] <- 0
cut_net$dR[] <- 0
cut_net$C <- c(AAA = 10, BBB = 0, CCC = 0)
cut_net$shortage <- c(AAA = 10, BBB = 0, CCC = 0)
cut_once <- cut_consumption(cut_net, amount = c(AAA = 25, BBB = 0, CCC = 0))
check(abs(cut_once$C["AAA"]) < 1e-8, "consumption cut stops at zero domestic use")

# Export ban: fraction 1 removes the whole row and banks it once.
ban <- apply_export_bans(E, reserves = c(0, 0, 0), c(AAA = 1, BBB = 0.25, CCC = 0))
check(all(abs(ban$E["AAA", ]) < 1e-8), "a full export ban zeroes that row")
check(abs(ban$reserves[1] - 40) < 1e-8, "a full ban adds the removed exports to reserves once")
check(abs(sum(ban$E["BBB", ]) - 60) < 1e-8, "a 25 percent ban keeps 75 percent of exports")
check(abs(ban$reserves[2] - 20) < 1e-8, "a 25 percent ban banks exactly the removed volume")

# Self-loops are refused.
looped <- base
diag(looped$E) <- 1
loop_err <- tryCatch(
    sim_cascade_PTA(looped, dP = c(AAA = -1, BBB = 0, CCC = 0), rfrac = 0.5, verbose = FALSE),
    error = function(e) e
)
check(inherits(loop_err, "error"), "nonzero diagonal stops the cascade")

check(is.na(safe_ratio(1, 0)), "division by zero baseline consumption is NA")
frac <- clamp_shock_fraction(c(1.5, -2, 0.2), c("AAA", "BBB", "CCC"), "toy")
check(identical(frac, c(1, -1, 0.2)), "shock fractions clamp to [-1, 1]")

diag_msg <- sim_diagnostics(list(
    P0 = P, E0 = E, dP = c(0, 0, 0), dR = c(0, 0, 0), dC = c(0, 0, 0), dE = E * 0
))
check(grepl("No aggregate", diag_msg), "a zero shock does not divide by zero in diagnostics")

if (length(failures)) {
    stop(paste(failures, collapse = "\n"))
}
message("All safety checks passed.")

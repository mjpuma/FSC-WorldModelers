# The stock-then-split option keeps the loss in proportion to use and exports.
#   Rscript tests/test_stock_then_split.R

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
    0, 0, 40,
    0, 0, 0,
    0, 0, 0
), 3, byrow = TRUE, dimnames = list(iso, iso))
P <- c(AAA = 100, BBB = 50, CCC = 10)
R <- c(AAA = 0, BBB = 0, CCC = 0)
net <- list(
    P = P, R = R, E = E,
    dR = c(AAA = 0, BBB = 0, CCC = 0),
    C = P + colSums(E) - rowSums(E),
    nc = 3,
    shortage = c(AAA = 0, BBB = 0, CCC = 0)
)
dP <- c(AAA = -30, BBB = 0, CCC = 0)

split <- sim_cascade_split(net, dP, rfrac = 0, verbose = FALSE)
check(isTRUE(split$cascade_converged), "split records convergence")
check(abs(split$C["AAA"] - 42) < 1e-6, "home use falls by the domestic share, 18")
check(abs(split$C["CCC"] - 38) < 1e-6, "the partner absorbs the export share, 12")
check(abs(split$E["AAA", "CCC"] - 28) < 1e-6, "the existing export link falls by 12")
check(sum(split$E[split$E > 1 & E <= 1e-8]) == 0, "split opens no new link")
check(all(split$E >= -1e-8) && all(split$C >= -1e-8), "split quantities stay non-negative")

covered <- sim_cascade_split(list(
    P = P, R = c(AAA = 30, BBB = 0, CCC = 0), E = E,
    dR = c(AAA = 0, BBB = 0, CCC = 0),
    C = P + colSums(E) - rowSums(E),
    nc = 3, shortage = c(AAA = 0, BBB = 0, CCC = 0)
), dP, rfrac = 1, verbose = FALSE)
check(abs(covered$C["AAA"] - 60) < 1e-6 && abs(covered$C["CCC"] - 50) < 1e-6,
      "reserves that cover the loss leave domestic use unchanged")
check(abs(covered$R["AAA"]) < 1e-6, "the covering reserve is drawn")

iso4 <- c("AAA", "BBB", "CCC", "DDD")
E4 <- matrix(0, 4, 4, dimnames = list(iso4, iso4))
E4["DDD", "AAA"] <- 10
P4 <- c(AAA = 100, BBB = 50, CCC = 10, DDD = 0)
R4 <- c(AAA = 0, BBB = 0, CCC = 0, DDD = 100)
C4 <- P4 + colSums(E4) - rowSums(E4)
neg <- list(
    P = P4, R = R4, E = E4,
    dR = setNames(rep(0, 4), iso4),
    C = C4, nc = 4,
    shortage = setNames(rep(0, 4), iso4)
)
held <- sim_cascade_split(neg, setNames(rep(0, 4), iso4), rfrac = 1, verbose = FALSE)
check(abs(held$C["DDD"] - C4["DDD"]) < 1e-6, "a negative baseline use is left unchanged")
check(abs(held$R["DDD"] - 100) < 1e-6, "stocks are not drawn to fill a negative baseline use")
check(abs(held$E["DDD", "AAA"] - 10) < 1e-6, "exports that create a negative baseline use stay in place")

pta <- sim_cascade_PTA(net, dP, rfrac = 0, verbose = FALSE, freeze_empty_reserves = FALSE)
check(abs(pta$C["AAA"] - split$C["AAA"]) > 1, "modified PTA does not copy the split")

if (length(failures)) stop(paste(failures, collapse = "\n"))
message("All stock-then-split checks passed")

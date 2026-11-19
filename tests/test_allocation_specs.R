# The three allocation settings stay distinct, and none opens a new link.
#   Rscript tests/test_allocation_specs.R

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
    0, 0, 10,
    0, 0, 0
), 3, byrow = TRUE, dimnames = list(iso, iso))
P <- c(AAA = 100, BBB = 80, CCC = 5)
R <- c(AAA = 0, BBB = 0, CCC = 0)
net <- list(
    P = P, R = R, E = E,
    dR = c(AAA = 0, BBB = 0, CCC = 0),
    C = P + colSums(E) - rowSums(E),
    nc = 3,
    shortage = c(AAA = 0, BBB = 0, CCC = 0)
)
dP <- c(AAA = -30, BBB = 0, CCC = 0)

published <- sim_cascade_PTA(net, dP, rfrac = 0, verbose = FALSE, freeze_empty_reserves = TRUE)
modified <- sim_cascade_PTA(net, dP, rfrac = 0, verbose = FALSE, freeze_empty_reserves = FALSE)
rta <- sim_cascade_RTA(net, dP, rfrac = 0, verbose = FALSE, rexp = 0.2)

check(isTRUE(published$cascade_converged), "published PTA records convergence")
check(isTRUE(rta$cascade_converged), "RTA records convergence")
check(published$cascade_iterations >= 1, "published PTA records an iteration count")
# This three-country loop keeps adjusting when nobody is frozen. The cap is reported.
check(!isTRUE(modified$cascade_converged) && modified$cascade_iterations == 50,
      "modified PTA reports the iteration cap when this toy loop does not settle")

# BBB to CCC can rise only when empty reserves do not freeze every exporter.
check(modified$E[2, 3] > net$E["BBB", "CCC"] + 1, "modified PTA reroutes along an existing link")
check(published$E["BBB", "CCC"] <= net$E["BBB", "CCC"] + 1e-6, "published PTA does not raise exports when reserves are empty")
check(rta$E["BBB", "CCC"] <= net$E["BBB", "CCC"] + 1e-6, "RTA with no reserves does not raise exports")

new_links <- function(E0, E1) sum(E0 <= 1e-8 & E1 > 1)
check(new_links(net$E, published$E) == 0, "published PTA opens no new link")
check(new_links(net$E, modified$E) == 0, "modified PTA opens no new link")
check(new_links(net$E, rta$E) == 0, "RTA opens no new link")
check(all(published$E >= -1e-8) && all(modified$E >= -1e-8) && all(rta$E >= -1e-8), "trade stays non-negative")
check(abs(published$P["AAA"] - 70) < 1e-6, "the 30 unit production loss is applied")
again <- sim_cascade_PTA(net, dP, rfrac = 0, verbose = FALSE, freeze_empty_reserves = TRUE)
check(isTRUE(all.equal(published$E, again$E)), "the same published PTA case repeats")

if (length(failures)) {
    stop(paste(failures, collapse = "\n"))
}
message("All allocation-spec checks passed")

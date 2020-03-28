## Functions for the Food Shocks Cascade (FSC) model

# Calculate net supply (P + E - I - dR) for each country in a food_net object
get_supply <- function(food_net) {
    food_net$P + colSums(food_net$E) - rowSums(food_net$E) - food_net$dR
}

# Update shortage values in food_net object
update_shortage <- function(food_net) {
    food_net$shortage <- food_net$C - get_supply(food_net)
    # Checks if any country gets additional supply (negative shortage) 
    #  which would suggest error in model
    if(any(food_net$shortage < -1)) stop("Negative shortage.")
    food_net
}

# Absorb any shortage less than a fraction eps of consumption (by country)
absorb_tiny_shortages <- function(food_net, eps = 1E-5) {
    food_net$C <- food_net$C - 
        ifelse(food_net$shortage < eps * food_net$C, food_net$shortage, 0)
    update_shortage(food_net)
} 

# Draw from reserves (per country) based on need and available reserves
draw_reserves <- function(food_net, need = food_net$shortage, avail = food_net$R) {
    food_net$dR <- food_net$dR - pmin(need, avail)
    food_net$R <- food_net$R - pmin(need, avail)
    update_shortage(food_net)
}

# Cut consumption by given amount (by country)
cut_consumption <- function(food_net, amount = food_net$shortage) {
    food_net$C <- food_net$C - amount
    update_shortage(food_net)
}

# Reallocate trade for countries based on additional supply needed (need)
# - Countries with need decrease exports and increase imports by the same proportion
#   across all existing trade links
# - exp_freeze is a condition to freeze exports (i.e. refuse requests to increase exports)
#   (by default, countries with no reserves cannot export more)
realloc_trade_prop_link <- function(food_net, need = food_net$shortage, 
                                    exp_freeze = food_net$R == 0) {
    # Determine available trade volume and proportional change factor
    Tavail <- rowSums(food_net$E) + colSums(food_net$E[!exp_freeze, ])
    prop_dT <- ifelse(Tavail == 0, 0, pmin(need / Tavail, 1))
    
    # Apply proportion to all links (except incoming links that froze exports)
    # Calculate prop_dE[i,j] = (!exp_freeze[i]) * prop_dT[j] - prop_dT[i]
    prop_dE <- outer(!exp_freeze, prop_dT) %>%
        sweep(1, prop_dT, `-`)
    
    food_net$E <- (1 + prop_dE) * food_net$E
    update_shortage(food_net)
}

# Reduce exports for all countries based on additional supply needed (need),
#  applying the same proportional reduction to all outbound links
reduce_exp_prop_link <- function(food_net, need = food_net$shortage) {
    # Determine proportion of exports to reduce
    Esum <- rowSums(food_net$E)
    prop_dE <- ifelse(Esum == 0, 0, pmin(need / Esum, 1))
    
    # Apply proportion to all links
    food_net$E <- sweep(food_net$E, 1, 1 - prop_dE, `*`)
    update_shortage(food_net)
}

# Increase imports based on amount needed by importers and amount available from exporters
#   Iteratively distribute available amounts proportionally to existing trade
#   with partners needing more
increase_imp_prop_resv <- function(food_net, need = food_net$shortage,
                                   avail = food_net$R) {
    dE <- matrix(0, nrow = food_net$nc, ncol = food_net$nc)
    while(any(need > 0)) {
        # E matrix with only importers who need more
        E_w_need <- sweep(food_net$E, 2, need > 0, `*`)
        # Distribute available amount proportionally to existing trade
        prop_dE <- ifelse(rowSums(E_w_need) == 0, 0, avail / rowSums(E_w_need))
        E_offer <- sweep(E_w_need, 1, prop_dE, `*`)
        # When total offer greater than importer's need, reduce all offers proportionally
        prop_dI <- ifelse(colSums(E_offer) == 0, 0, pmin(need / colSums(E_offer), 1))
        E_accept <- sweep(E_offer, 2, prop_dI, `*`)
        # If no change in trade, exit loop
        if(sum(E_accept) < 1) break
        # Update need, avail and total dE
        need <- need - colSums(E_accept)
        avail <- avail - rowSums(E_accept)
        dE <- dE + E_accept
    }
    food_net$E <- food_net$E + dE
    update_shortage(food_net)
}

# Dignostic: This function takes the output of sim_cascade (or sim_1c)
#  and tests whether it respects equation: S = P + I - E = R + C
sim_diagnostics <- function(sim_res, tol = 1E-5) {
  S0 <- sim_res$P0 + colSums(sim_res$E0) - rowSums(sim_res$E0) 
  dS0 <- sum(sim_res$dP)
  cnames <- names(sim_res$P0)
  
  # Test 1: Total dR + dC must match initial shock
  # (within relative difference of tol)
  discrep <- (sum(sim_res$dR) + sum(sim_res$dC))/dS0 - 1
  if (abs(discrep) > tol)
    return(paste("Total dC + dR does not match initial shock.",
                 "Relative difference:", discrep))
  
  # Test 2: After initial shock, dI - dE = dR + dC by country
  # (within relative difference of tol * final net supply)
  dS_byc <- sim_res$dP + colSums(sim_res$dE) - rowSums(sim_res$dE)
  discrep <- which(abs((dS_byc - sim_res$dR - sim_res$dC)/(dS_byc + S0)) > tol)
  if (length(discrep) > 0)
    return(paste("Net supply change does not match dC + dR for countries:",
                 paste(cnames[discrep], collapse = ", ")))
  
  return("All tests passed.")
}
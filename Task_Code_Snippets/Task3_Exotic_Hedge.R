# ==============================================================================
# TASK 3: KNOCK-IN SPARK SPREAD HEDGE VALUATION & CORRELATION AUDIT
# ==============================================================================

# 1. DERIVATIVE PAYOFF LOGIC (THE "SPARK SPREAD" UNDERLYING)
# ------------------------------------------------------------------------------
# Theory: Payoff = Trigger(Wind) * Max(0, Gas - Elec) * Volume
# The Knock-In acts as a "conditional insurance" policy.

# A. Calculate the Spread Matrix (Gas - Electricity)
# Note: We exclude T=0 (row 1) as there is no payoff at the start date.
spread_matrix <- sim_gas[2:61, ] - sim_elec[2:61, ]

# B. Identify Knock-In Events (Utilization < 40%)
# This creates a Boolean matrix (TRUE/FALSE)
trigger_matrix <- sim_util[2:61, ] < 0.40

# C. Calculate Monthly Payoffs
# Volumetric obligation: 50MW * 720 hours = 36,000 MWh
mwh_volume    <- project_params$demand_ppa * project_params$hours_mo
payoff_matrix <- trigger_matrix * pmax(0, spread_matrix) * mwh_volume

# 2. DISCOUNTING & FAIR VALUE (BASE CASE)
# ------------------------------------------------------------------------------
# Discount Rate: 2% annually / 12 months
r_hedge        <- 0.02 / 12
discount_vec   <- 1 / (1 + r_hedge)^(1:n_months)

# Vector of 10,000 PVs (one for each simulated 'future')
path_hedge_values <- colSums(payoff_matrix * discount_vec)
## we use the mean here sincce we sum the discounted payoffs for all
## 10.000 simulations. after that we take the mean
fair_value_hedge  <- mean(path_hedge_values)

# 3. STRESS TEST: CORRELATION DECOUPLING (CORR = 0)
# ------------------------------------------------------------------------------
if (exists("stress_test_mode") && stress_test_mode == TRUE) {
  
  # A. The Shuffling Technique
  # We decouple Gas from Elec by randomizing path pairings.
  shuffled_indices <- sample(1:n_paths)
  sim_gas_stressed <- sim_gas[, shuffled_indices]
  
  # B. Recalculate Stressed Payoffs
  spread_stressed <- sim_gas_stressed[2:61, ] - sim_elec[2:61, ]
  payoff_stressed <- trigger_matrix * pmax(0, spread_stressed) * mwh_volume
  
  # C. Stressed Fair Value
  path_values_stressed <- colSums(payoff_stressed * discount_vec)
  fair_value_stressed  <- mean(path_values_stressed)
  
  # D. AUDIT: Statistical Verification of Correlation Breakdown
  # We convert matrices to vectors to check 'Global Correlation'
  corr_base     <- cor(as.vector(sim_gas[2:61, ]), as.vector(sim_elec[2:61, ]))
  corr_stressed <- cor(as.vector(sim_gas_stressed[2:61, ]), as.vector(sim_elec[2:61, ]))
}

# ------------------------------------------------------------------------------
# 4. TASK 3 OUTPUT SUMMARY & AUDIT REPORT
# ------------------------------------------------------------------------------
cat("\n================================================================\n")
cat("TASK 3: HEDGE VALUATION & STATISTICAL AUDIT\n")
cat("================================================================\n")

cat(sprintf("Fair Value (Base Case):      €%s\n", format(round(fair_value_hedge, 2), big.mark=",")))
cat(sprintf("- Base Corr (Elec/Gas):      %s\n", round(corr_base, 4)))

if (exists("stress_test_mode") && stress_test_mode == TRUE) {
  cat(sprintf("Fair Value (Stress Test):    €%s\n", format(round(fair_value_stressed, 2), big.mark=",")))
  cat(sprintf("Correlation Impact (Value):  €%s\n", format(round(fair_value_stressed - fair_value_hedge, 2), big.mark=",")))
  cat("----------------------------------------------------------------\n")
  cat("CORRELATION VERIFICATION:\n")
  cat(sprintf("- Base Corr (Elec/Gas):      %s\n", round(corr_base, 4)))
  cat(sprintf("- Stressed Corr (Elec/Gas):  %s (Approaching Zero)\n", round(corr_stressed, 4)))
  
  # Theoretical Insight
  cat("\nTHEORETICAL INSIGHT:\n")
  if(fair_value_stressed > fair_value_hedge) {
    cat("The hedge is CHEAPER when correlated. High correlation means Gas and\n")
    cat("Elec move together, naturally 'shrinking' the spread payoffs.\n")
  } else {
    cat("The hedge is MORE EXPENSIVE when correlated. This suggests the \n")
    cat("spread widens under historical market conditions.\n")
  }
}
cat("================================================================\n")

# ==============================================================================
# DATA LINEAGE & VARIABLE ANCESTRY (TASK 1 -> TASK 2 -> TASK 3)
# ==============================================================================
# To ensure mathematical consistency across the project, all Task 3 variables 
# are derived from a single "Source of Truth" established in Tasks 1 and 2:
#
# 1. THE ANCHORS (T=0: January 2026):
#    - p_elec_0 (135.02), p_gas_0 (75.705), p_carb_0 (79.8), u_wind_0 (0.3789).
#    - These are the last known real-world data points from your Excel 'Last_Price'.
#
# 2. THE SIMULATION ENGINES (Task 1.2):
#    - sim_elec & sim_gas: Generated via Geometric Brownian Motion (GBM). 
#      They evolve from the T=0 Anchors using the Drift (mu) and Volatility (sigma) 
#      calculated from the historical 'Returns' dataframe.
#    - sim_util: Generated via an Additive Random Walk in Logit-Space (l_wind_0).
#      The results were transformed back to [0,1] using the Sigmoid function:
#      inv_logit = exp(L) / (1 + exp(L)).
#
# 3. THE REVENUE STACK (Task 2):
#    - Used sim_util and sim_elec to calculate 'rev_total'.
#    - Established the 50MW PPA / 125MW Capacity relationship.
#
# 4. THE HEDGE UNDERLYING (Task 3):
#    - sim_gas & sim_elec: Used here to calculate the "Spark Spread" (Gas - Elec).
#    - sim_util: Used here to trigger the "Knock-In" (Shortfall if Wind < 40%).
#
# VERDICT: By using these forecast matrices instead of historical 'shuffling',
# Task 3 evaluates the hedge against 10,000 "Possible Futures" rather than 
# simply re-playing the past.
# ==============================================================================
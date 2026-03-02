# ==============================================================================
# TASK 3: KNOCK-IN SPARK SPREAD HEDGE VALUATION
# ==============================================================================

# 1. DEFINE DERIVATIVE PAYOFF LOGIC
# ------------------------------------------------------------------------------
# The hedge only pays out if:
#   1. Wind < 40% (Knock-in Trigger)
#   2. Gas Price > Electricity Price (Positive Spark Spread)

# A. Calculate the Spread Matrix (10,000 x 60)
# Formula: S_t = Gas - Electricity
spread_matrix <- sim_gas - sim_elec

# B. Identify Knock-In Events
# Logic: Trigger = 1 if Wind < 40% (0.40), else 0
trigger_matrix <- sim_util < 0.40

# C. Calculate Monthly Payoffs
# Formula: Payoff = Trigger * max(0, Spread) * MWh_Obligation
mwh_volume <- project_params$demand_ppa * project_params$hours_mo # 36,000 MWh

payoff_matrix <- trigger_matrix * pmax(0, spread_matrix) * mwh_volume

# 2. FAIR VALUE CALCULATION (NPV OF HEDGE)
# ------------------------------------------------------------------------------
# We discount the payoffs for each path and then take the average.
# Discount Rate: 2% annual (0.166% monthly) as per your old script
r_hedge <- 0.02 / 12
discount_hedge <- 1 / (1 + r_hedge)^(1:n_months)

# Calculate PV for each of the 10,000 paths
path_hedge_values <- colSums(payoff_matrix[2:61, ] * discount_hedge)

# The Fair Value is the average cost across all possible futures
fair_value_hedge <- mean(path_hedge_values)


# ------------------------------------------------------------------------------
# 3. STRESS TEST (ZERO CORRELATION)
# ------------------------------------------------------------------------------
# In a "Stress Test," we assume Gas and Elec move independently. 
# To simulate this, we shuffle the Gas paths so they no longer 'track' Elec.
if (exists("stress_test_mode") && stress_test_mode == TRUE) {
  cat("Applying Stress Test: Decoupling Gas and Electricity correlations...\n")
  
  # Shuffle the indices of the 10,000 paths for Gas only
  shuffled_indices <- sample(1:n_paths)
  sim_gas_stressed <- sim_gas[, shuffled_indices]
  
  # Re-calculate payoffs with independent variables
  spread_stressed <- sim_gas_stressed - sim_elec
  payoff_stressed <- trigger_matrix * pmax(0, spread_stressed) * mwh_volume
  
  path_values_stressed <- colSums(payoff_stressed[2:61, ] * discount_hedge)
  fair_value_stressed  <- mean(path_values_stressed)
}

# ------------------------------------------------------------------------------
# TASK 3 OUTPUT SUMMARY
# ------------------------------------------------------------------------------
cat("\n--- TASK 3: HEDGE VALUATION SUMMARY ---\n")
cat("Fair Value (Base Case):    €", round(fair_value_hedge, 2), "\n")
if (exists("fair_value_stressed")) {
  cat("Fair Value (Stress Test):  €", round(fair_value_stressed, 2), "\n")
  cat("Correlation Impact:        €", round(fair_value_stressed - fair_value_hedge, 2), "\n")
}
cat("---------------------------------------\n")
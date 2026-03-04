# ==============================================================================
# TASK 2 & 4: CASHFLOW WATERFALL & EQUITY VALUATION
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. DATA LINEAGE: ROUTING BASED ON MODE LABEL
# ------------------------------------------------------------------------------
# Routes the script to use either the Base or Stressed simulation universe.
message("Initiating Case Specific Variables")

if (mode_label == "UNHEDGED") {
  univ       <- sim_universe_base
  run_hedge  <- FALSE
  hedge_cost <- 0
  cat(">>> DATA LOADED: Base Universe (Unhedged)\n")
  
} else if (mode_label == "HEDGED") {
  univ       <- sim_universe_base
  run_hedge  <- TRUE
  hedge_cost <- hedge_base$fair_value
  cat(">>> DATA LOADED: Base Universe + Hedge Payouts (Hedged)\n")
  
} else if (mode_label == "STRESSED UNHEDGED") {
  univ       <- sim_universe_stressed
  run_hedge  <- FALSE
  hedge_cost <- 0
  cat(">>> DATA LOADED: Stressed Universe (Unhedged)\n")
  
} else {
  stop("FATAL ERROR: Invalid mode_label passed to Calculation script.")
}

# ------------------------------------------------------------------------------
# 2. FINANCIAL PARAMETERS & BASE ASSUMPTIONS
# ------------------------------------------------------------------------------
message("Initiating base Parameters")
n_paths  <- ncol(univ$sim_elec)
n_months <- 60 # 5 Years

# Unpack centralized project_params (Make sure these are updated in Data Prep!)
cap_wind        <- project_params$cap_wind
cap_gas         <- project_params$cap_gas
demand_ppa      <- project_params$demand_ppa
hours_mo        <- project_params$hours_mo
debt_principal  <- project_params$debt_principal
monthly_coupon  <- project_params$monthly_coupon
distress_fee    <- project_params$distress_fee

# Unpack Operational Assumptions
wind_opex_mwh   <- project_params$wind_opex_mwh   
gas_opex_mwh    <- project_params$gas_opex_mwh    
emission_factor <- project_params$emission_factor 

# Capital Structure
equity_upfront  <- 40 * 1e6 # €40 Million

# Discrete Compounding Rates
r_annual_rf  <- 0.02 # 2% Annual Risk-free rate
r_annual_eq  <- 0.12 # 12% Expected Return on Equity

# Convert Annual to Monthly Discrete Rates
r_monthly_rf <- (1 + r_annual_rf)^(1/12) - 1
r_monthly_eq <- (1 + r_annual_eq)^(1/12) - 1

# Pre-allocate tracking matrices
A_t               <- matrix(0, nrow = n_months + 1, ncol = n_paths)
equity_injections <- matrix(0, nrow = n_months, ncol = n_paths)
monthly_nocf      <- matrix(0, nrow = n_months, ncol = n_paths) # <-- ADDED THIS LINE

# Initial balance at Month 0 is exactly €0
A_t[1, ] <- 0 

# ------------------------------------------------------------------------------
# 3. THE 60-MONTH WATERFALL LOOP
# ------------------------------------------------------------------------------
message("Starting the 60 Month Calculation")

for (t in 1:n_months) {
  
  # --- A. PRODUCTION LOGIC ---
  wind_gen_mw <- univ$sim_util[t+1, ] * cap_wind
  
  # Priority: Data Center is supplied first by Wind
  wind_to_dc  <- pmin(wind_gen_mw, demand_ppa)
  
  # Backup: Gas Peaker fills exact deficit if Wind < 50 MW
  gas_to_dc   <- pmax(demand_ppa - wind_gen_mw, 0)
  
  # Excess: Any wind generation > 50 MW
  excess_wind <- pmax(wind_gen_mw - demand_ppa, 0)
  
  # --- B. REVENUE LOGIC (UPDATED TO MATCH PDF PROMPT) ---
  # Data Center priced at the FLOATING Monthly Average Spot Price with 20% discount
  rev_dc       <- (wind_to_dc + gas_to_dc) * hours_mo * (univ$sim_elec[t+1, ] * 0.80)
  
  # Excess sold to grid at Monthly Average Spot Price (no discount)
  rev_merchant <- excess_wind * hours_mo * univ$sim_elec[t+1, ]
  
  total_rev    <- rev_dc + rev_merchant
  
  # --- C. OPERATING COSTS ---
  # Wind maintenance and operating costs of €20/MWh
  cost_wind   <- wind_gen_mw * hours_mo * wind_opex_mwh
  
  # Gas operating costs are €45/MWh + direct fuel cost + carbon costs
  # Gas price is quoted directly in MWh, no heat rate needed
  cost_gas    <- gas_to_dc * hours_mo * (
    gas_opex_mwh + 
      univ$sim_gas[t+1, ] + 
      (univ$sim_carb[t+1, ] * emission_factor)
  )
  
  total_opex  <- cost_wind + cost_gas
  
  # --- D. HEDGE PAYOUT ---
  payout_hedge <- if (run_hedge) hedge_base$payoff_matrix[t, ] else 0
  
  # --- E. NET CASH FLOW ---
  net_cf <- total_rev + payout_hedge - total_opex - monthly_coupon
  monthly_nocf[t, ] <- net_cf # <-- ADDED THIS LINE
  
  # --- F. CASH ACCOUNT & DISTRESS MECHANISM ---
  # Balance earns risk-free rate monthly
  potential_balance <- (A_t[t, ] * (1 + r_monthly_rf)) + net_cf
  
  # Check for deficits
  deficit_paths <- potential_balance < 0
  
  # Emergency Injection to bring balance back to zero
  equity_injections[t, deficit_paths] <- abs(potential_balance[deficit_paths])
  
  # Reset deficit paths to €0, profitable paths keep positive cash
  A_t[t+1, ] <- pmax(potential_balance, 0)
}



# ------------------------------------------------------------------------------
# 4. FINAL EQUITY VALUATION (NPV)
# ------------------------------------------------------------------------------
message("Calculating the NPV")
# Discount cash flows using the given cost of equity
months_vec       <- 1:n_months
discount_factors <- 1 / (1 + r_monthly_eq)^months_vec

# --- A. Final Dividend to Equity (Month 60) ---
# Payoff = max(0, A_60 - Principal Repayment)
final_payoff    <- pmax(0, A_t[61, ] - debt_principal)
pv_final_payoff <- final_payoff * discount_factors[60]

# --- B. Present Value of Emergency Injections ---
# Cost of Distress: Equity Holders pay €1.10 for every €1.00 injected
pv_injections   <- colSums(equity_injections * (1 + distress_fee) * discount_factors)

# --- C. Net Present Value of the SPV ---
# Expected NPV of Equity requires deducting PV of injections
# If hedged, deduct the upfront cost of the derivative strip
path_npvs       <- pv_final_payoff - pv_injections - equity_upfront - hedge_cost

# ------------------------------------------------------------------------------
# 5. RISK METRICS EXTRACTION
# ------------------------------------------------------------------------------
# Q2c: Probability of Distress: percentage of months across all simulations
total_distress_months <- sum(equity_injections > 0)
prob_distress_months  <- total_distress_months / (n_months * n_paths)

# Q2d: Cumulative Cost of Distress
# Calculates just the 10% penalty fee explicitly
cum_cost_distress_penalty <- colSums(equity_injections * distress_fee)

# ------------------------------------------------------------------------------
# 6. ARCHIVE RESULTS 
# ------------------------------------------------------------------------------
message("Archiving Results")
current_output <- list(
  npv_dist          = path_npvs,
  expected_npv      = mean(path_npvs),
  prob_distress     = prob_distress_months,
  cum_cost_distress = cum_cost_distress_penalty,
  a_t_matrix        = A_t,
  monthly_nocf      = monthly_nocf # <-- ADDED THIS LINE
)

if (mode_label == "UNHEDGED") {
  results_unhedged <- current_output
} else if (mode_label == "HEDGED") {
  results_hedged <- current_output
} else if (mode_label == "STRESSED UNHEDGED") {
  results_stressed <- current_output
}

# ------------------------------------------------------------------------------
# 7. Q2.b / Q2.d / Q5.b: FINAL REPORTS
# ------------------------------------------------------------------------------
# 95% confidence interval for highest and lowest NPV
npv_95_bounds <- quantile(path_npvs, probs = c(0.025, 0.975))

expected_npv                <- mean(path_npvs)
prob_distress_monthly       <- prob_distress_months
cumulative_cost_of_distress <- cum_cost_distress_penalty

cat("\n==========================================\n")
cat("PROJECT SUMMARY:", mode_label, "\n")
cat("==========================================\n")
cat(sprintf("Expected Equity NPV:           €%s\n", format(round(expected_npv, 2), big.mark=",")))
cat(sprintf("95%% Conf. Interval:             €%s to €%s\n", 
            format(round(npv_95_bounds[1], 2), big.mark=","), 
            format(round(npv_95_bounds[2], 2), big.mark=",")))
cat(sprintf("Monthly Prob. of Distress:     %s%%\n", round(prob_distress_monthly * 100, 2)))
cat(sprintf("Avg. Cumulative Distress Cost: €%s\n", format(round(mean(cumulative_cost_of_distress), 2), big.mark=",")))
cat("==========================================\n")
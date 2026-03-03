# ==============================================================================
# UNIFIED TASK 2 & 4: CONSOLIDATED CASH FLOW WATERFALL (STRESS-TEST READY)
# ==============================================================================

# ------------------------------------------------------------------------------
# 2a & 4a. PHYSICAL PRODUCTION & REVENUE WATERFALL
# Theory: NOCF = (Fixed Revenue + Merchant Revenue) - Shortfall Costs + Hedge Payoff
# ------------------------------------------------------------------------------

# 1. Physical Generation (125MW capacity)
# Math: Production = Capacity * Utilization_t * Hours_in_month
prod_total_mwh <- project_params$cap_wind * sim_util * project_params$hours_mo
target_mwh     <- project_params$demand_ppa * project_params$hours_mo

# 2. Revenue from PPA (Fixed Price)
# Math: Sales capped at 50MW. Revenue = min(Actual, Target) * Fixed_Price
prod_ppa_mwh   <- pmin(prod_total_mwh, target_mwh)
rev_ppa        <- prod_ppa_mwh * p_ppa

# 3. Revenue from Grid (Merchant Upside)
# Math: Any excess wind energy is sold at the simulated spot electricity price (sim_elec)
prod_merch_mwh <- pmax(0, prod_total_mwh - target_mwh)
rev_merchant   <- prod_merch_mwh * sim_elec

# 4. Fulfillment Cost (The "Shortfall" - Task 4a)
# Math: If Wind < 50MW, SPV buys the missing volume at sim_elec to fulfill the PPA.
# This represents the "Natural" operational risk of the project.
shortfall_mwh  <- pmax(0, target_mwh - prod_total_mwh)
cost_shortfall <- shortfall_mwh * sim_elec

# 5. Consolidated Base Cash Flow (Unhedged)
# Math: Net Cash = Revenue - Out-of-pocket Shortfall Costs
base_nocf <- rev_ppa + rev_merchant - cost_shortfall

# ------------------------------------------------------------------------------
# 6. SCENARIO SELECTOR (BASE vs HEDGED vs STRESSED UNHEDGED)
# ------------------------------------------------------------------------------
# We use logic gates to determine which "Universe" we are calculating.
# ------------------------------------------------------------------------------
# 6. SCENARIO SELECTOR (REWORKED FOR MATRIX CONFORMITY)
# ------------------------------------------------------------------------------

# Define the Forecast Horizon (Excluding T=0)
forecast_idx <- 2:61

if (exists("stress_test_mode") && stress_test_mode == TRUE) {
  # STRESSED UNHEDGED
  nocf_matrix <- base_nocf[forecast_idx, ]
  mode_label  <- "STRESSED UNHEDGED"
  corr_level  <- "0.00 (Decoupled)"
  
} else if (exists("hedge_cashflow_calculator") && hedge_cashflow_calculator == TRUE) {
  # HEDGED 
  # We ensure base_nocf is sliced to 60 rows to match payoff_matrix
  nocf_matrix <- base_nocf[forecast_idx, ] + payoff_matrix
  mode_label  <- "HEDGED"
  corr_level  <- "Historical (GBM)"
  
} else {
  # PURE UNHEDGED
  nocf_matrix <- base_nocf[forecast_idx, ]
  mode_label  <- "UNHEDGED"
  corr_level  <- "Historical (GBM)"
}

# ------------------------------------------------------------------------------
# 2b & 4b. EXPECTED NET PRESENT VALUE (NPV)
# Theory: NPV = Σ [ CashFlow_t / (1 + r)^t ]
# ------------------------------------------------------------------------------
r_monthly        <- 0.10 / 12
discount_factors <- 1 / (1 + r_monthly)^(1:n_months)

# path_npv: Summing discounted monthly flows for all 10,000 simulated paths
path_npv     <- colSums(nocf_matrix[2:61, ] * discount_factors)
expected_npv <- mean(path_npv)

# ------------------------------------------------------------------------------
# 2c & 4b. PROBABILITY OF DISTRESS (MONTHLY)
# Theory: Probability = (Count of months where CashFlow < Debt) / Total observations
# ------------------------------------------------------------------------------
debt_service <- project_params$monthly_coupon
distress_matrix <- nocf_matrix[2:61, ] < debt_service
prob_distress_monthly <- mean(distress_matrix)

# ------------------------------------------------------------------------------
# 2d & 4b. CUMULATIVE PROBABILITY OF DISTRESS
# Theory: The chance that a project fails AT LEAST ONCE during its 5-year life.
# ------------------------------------------------------------------------------
ever_failed       <- apply(distress_matrix, 2, cumsum) > 0
cum_prob_distress <- rowMeans(ever_failed)

# ------------------------------------------------------------------------------
# DYNAMIC VARIABLE ARCHIVING
# ------------------------------------------------------------------------------
if (mode_label == "STRESSED UNHEDGED") {
  path_npv_stressed          <- path_npv
  expected_npv_stressed      <- expected_npv
  cum_prob_distress_stressed <- cum_prob_distress
  prob_distress_stressed     <- prob_distress_monthly
} else if (mode_label == "HEDGED") {
  path_npv_hedged            <- path_npv
  expected_npv_hedged        <- expected_npv
  cum_prob_distress_hedged   <- cum_prob_distress
  prob_distress_hedged       <- prob_distress_monthly
} else {
  path_npv_unhedged          <- path_npv
  expected_npv_unhedged      <- expected_npv
  cum_prob_distress_unhedged <- cum_prob_distress
  prob_distress_unhedged     <- prob_distress_monthly
}

# ==============================================================================
# FINAL CONSOLIDATED SUMMARY
# ==============================================================================
cat("\n---", mode_label, "PROJECT SUMMARY ---\n")
cat("Correlation Level:        ", corr_level, "\n")
cat("Expected NPV:             €", round(expected_npv, 2), "\n")
cat("Monthly Prob. of Distress:", round(prob_distress_monthly * 100, 2), "%\n")
cat("Final Cumulative Risk:    ", round(tail(cum_prob_distress, 1) * 100, 2), "%\n")
cat("------------------------------------------\n")
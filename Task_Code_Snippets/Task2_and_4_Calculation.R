# ==============================================================================
# UNIFIED TASK 2, 4 & 5: CONSOLIDATED CASH FLOW WATERFALL
# ==============================================================================

# ------------------------------------------------------------------------------
# Q2.a / Q4.a: PHYSICAL PRODUCTION & REVENUE WATERFALL
# ------------------------------------------------------------------------------

# [Q2.a.i]: Revenue from Electricity Sales (Data Center + Excess)
prod_total_mwh <- project_params$cap_wind * sim_util * project_params$hours_mo
target_mwh     <- project_params$demand_ppa * project_params$hours_mo

# Data Center Sales (Fixed PPA volume)
prod_ppa_mwh   <- pmin(prod_total_mwh, target_mwh)
rev_ppa        <- prod_ppa_mwh * p_ppa 

# Excess Sales (Merchant upside sold at spot price)
prod_merch_mwh <- pmax(0, prod_total_mwh - target_mwh)
rev_merchant   <- prod_merch_mwh * sim_elec 

# [Q2.a.ii]: Operating costs (Wind + backup fulfillment) and Fuel/Carbon Costs
# Shortfall occurs when Wind < PPA obligation; SPV must buy backup at market spot price.
shortfall_mwh  <- pmax(0, target_mwh - prod_total_mwh)
cost_shortfall <- shortfall_mwh * sim_elec 

# Fixed Maintenance/Land costs and Carbon penalties for grid-purchased backup
cost_opex_fixed <- 150000 
emission_factor <- 0.4 
cost_carbon     <- shortfall_mwh * emission_factor * sim_carb 

# [Q2.a.iii]: Net Operating Cash Flow (NOCF)
base_nocf <- (rev_ppa + rev_merchant) - (cost_shortfall + cost_opex_fixed + cost_carbon)

# ------------------------------------------------------------------------------
# SCENARIO SELECTOR: [Q2] Unhedged, [Q4] Hedged, [Q5] Stressed
# ------------------------------------------------------------------------------
forecast_idx <- 2:61
nocf_matrix  <- base_nocf[forecast_idx, ]

if (exists("stress_test_mode") && stress_test_mode == TRUE) {
  # [Q5.a/b]: Manual override of correlation to 0.0 (already handled in simulation engine)
  mode_label  <- "STRESSED UNHEDGED"
} else if (exists("hedge_cashflow_calculator") && hedge_cashflow_calculator == TRUE) {
  # [Q4.a]: Owning the derivative strip. Add monthly payoffs if knocked-in/exercised.
  nocf_matrix <- nocf_matrix + payoff_matrix
  mode_label  <- "HEDGED"
} else {
  # [Q2.a]: Standard Unhedged Evaluation
  mode_label  <- "UNHEDGED"
}

# ------------------------------------------------------------------------------
# Q2.a.iv, Q2.c, & Q2.d: DISTRESS EVENTS AND INJECTION COSTS
# ------------------------------------------------------------------------------
debt_service <- project_params$monthly_coupon

# [Q2.c]: Probability of Distress (Percentage of months across all simulations)
distress_matrix       <- nocf_matrix < debt_service 
prob_distress_monthly <- mean(distress_matrix)

# [Q2.a.iv]: Cost of required injections (including distress penalty)
# Math: Gap = Debt - Cash; Injection = Gap * 1.10
cash_gap_raw      <- pmax(0, debt_service - nocf_matrix)

# THE CONFORMABILITY FIX: Explicitly cast as a 60x10000 matrix
equity_injections <- matrix(cash_gap_raw * (1 + project_params$distress_fee), 
                            nrow = 60, ncol = n_paths)

# [Q2.d]: Distribution of the Cumulative Cost of Distress across simulations
cumulative_cost_of_distress <- colSums(equity_injections)

# ------------------------------------------------------------------------------
# Q2.a.v: FINAL DIVIDEND TO EQUITY (ROBUST VERSION)
# ------------------------------------------------------------------------------
# 1. Calculate base dividends (Excess cash above monthly interest)
# Ensure nocf_matrix and debt_service are subtracted correctly
dividends_raw <- pmax(0, nocf_matrix - debt_service)

# 2. THE FIX: Explicitly cast as a matrix to prevent "dimension dropping"
# This ensures R knows there are 60 rows and 10,000 columns
dividends <- matrix(dividends_raw, nrow = 60, ncol = n_paths)

# 3. Apply Bullet Principal Repayment at Month 60
# Now that it's a matrix, the [60, ] index will work perfectly
dividends[60, ] <- pmax(0, dividends[60, ] - project_params$debt_principal)

# 4. Net Shareholder Flow calculation
# [Q2.a.v]: Both are now forced 60x10000 matrices; the subtraction will now work.
shareholder_net_flow <- dividends - equity_injections

# ------------------------------------------------------------------------------
# Q2.b & Q4.b: EXPECTED NPV OF THE EQUITY
# ------------------------------------------------------------------------------
# [Q2.b]: Use discount rate of 12% for equity valuation
r_equity_annual  <- 0.12
r_monthly        <- r_equity_annual / 12
discount_factors <- 1 / (1 + r_monthly)^(1:n_months)

path_npv <- colSums(shareholder_net_flow * discount_factors)

# [Q4.b]: Deduct upfront Fair Value (premium) of the derivative at T=0
if (mode_label == "HEDGED") {
  path_npv <- path_npv - fair_value_hedge
}

# [Q2.b]: Expected NPV and 95% Confidence Bounds
expected_npv  <- mean(path_npv)
npv_95_bounds <- quantile(path_npv, probs = c(0.025, 0.975))

# ------------------------------------------------------------------------------
# Q2.c / Q4.c / Q5.c: CUMULATIVE RISK ARCHIVING (The Default Curve)
# ------------------------------------------------------------------------------
# 1. 'cumany' checks if a simulation path HAS EVER hit distress up to month T
# apply(..., 2, ...) runs this for each of the 10,000 paths
path_has_ever_defaulted <- apply(distress_matrix, 2, cumany)

# 2. rowMeans calculates the % of paths in default at each month (1 to 60)
cum_prob_distress_vector <- rowMeans(path_has_ever_defaulted)

# 3. Archive based on the active Scenario
if (mode_label == "UNHEDGED") {
  expected_npv_unhedged       <- expected_npv
  prob_distress_unhedged      <- prob_distress_monthly
  cum_prob_distress_unhedged  <- cum_prob_distress_vector # FIXES THE ERROR
} else if (mode_label == "HEDGED") {
  expected_npv_hedged         <- expected_npv
  prob_distress_hedged        <- prob_distress_monthly
  cum_prob_distress_hedged    <- cum_prob_distress_vector # FIXES THE ERROR
} else if (mode_label == "STRESSED UNHEDGED") {
  expected_npv_stressed       <- expected_npv
  prob_distress_stressed      <- prob_distress_monthly
  cum_prob_distress_stressed  <- cum_prob_distress_vector # FIXES THE ERROR
}

# ------------------------------------------------------------------------------
# Q2.d / Q5.b: FINAL REPORTS
# ------------------------------------------------------------------------------
cat("\n==========================================\n")
cat("PROJECT SUMMARY:", mode_label, "\n")
cat("==========================================\n")
cat(sprintf("Expected Equity NPV:          €%s\n", format(round(expected_npv, 2), big.mark=",")))
cat(sprintf("95%% Conf. Interval:           €%s to €%s\n", 
            format(round(npv_95_bounds[1], 2), big.mark=","), 
            format(round(npv_95_bounds[2], 2), big.mark=",")))
cat(sprintf("Monthly Prob. of Distress:     %s%%\n", round(prob_distress_monthly * 100, 2)))
cat(sprintf("Avg. Cumulative Distress Cost: €%s\n", format(round(mean(cumulative_cost_of_distress), 2), big.mark=",")))
cat("==========================================\n")
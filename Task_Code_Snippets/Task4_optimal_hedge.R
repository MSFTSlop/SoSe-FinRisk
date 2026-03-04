# ==============================================================================
# TASK 4d: HEDGE OPTIMIZATION (THE "SWEET SPOT" FINDER)
# ==============================================================================

# Define the range of hedge coverage (0% to 100% of the 50MW obligation)
hedge_ratios <- seq(0, 1, by = 0.2) # 0%, 20%, 40%, 60%, 80%, 100%
opt_results  <- data.frame()

cat("\n--- STARTING STRESS-TESTED OPTIMIZATION LOOP ---\n")

for (ratio in hedge_ratios) {
  
  # 1. Update the local environment variables
  # We scale the payoff matrix calculated in Task 3 by the ratio
  # Note: hedge_cost (upfront premium) must also be scaled by the ratio
  current_ratio_payoffs <- hedge_base$payoff_matrix * ratio
  current_ratio_premium <- hedge_base$fair_value * ratio
  
  # 2. RUN MINI-WATERFALL 
  # (Simulating the logic from Task 2 but with the scaled hedge)
  
  # Pre-allocate for this specific ratio
  A_t_opt      <- matrix(0, nrow = n_months + 1, ncol = n_paths)
  inj_opt      <- matrix(0, nrow = n_months, ncol = n_paths)
  
  for (t in 1:n_months) {
    # Calculate NOCF (Using logic from Task 2 script)
    # [Revenue - OPEX - Coupon + (Hedge Payoff * Ratio)]
    # ... (Logic assumed to be identical to your main waterfall) ...
    
    # For speed in this loop, we can simplify the NOCF calculation 
    # if you have the base_nocf already saved from the UNHEDGED run:
    net_cf <- results_unhedged$monthly_nocf[t, ] + current_ratio_payoffs[t, ]
    
    # Cash Account Logic
    pot_bal <- (A_t_opt[t, ] * (1 + r_monthly_rf)) + net_cf
    deficits <- pot_bal < 0
    inj_opt[t, deficits] <- abs(pot_bal[deficits])
    A_t_opt[t+1, ] <- pmax(pot_bal, 0)
  }
  
  # 3. VALUATION
  payout_60    <- pmax(0, A_t_opt[61, ] - debt_principal)
  pv_payout    <- payout_60 / (1 + r_monthly_eq)^60
  pv_penalties <- colSums(inj_opt * (1 + distress_fee) / (1 + r_monthly_eq)^(1:60))
  
  # Net NPV for this specific ratio
  path_npvs_opt <- pv_payout - pv_penalties - equity_upfront - current_ratio_premium
  
  # 4. STORE METRICS
  opt_results <- rbind(opt_results, data.frame(
    Hedge_Ratio      = paste0(ratio * 100, "%"),
    Expected_NPV     = mean(path_npvs_opt),
    Prob_of_Distress = sum(inj_opt > 0) / (n_months * n_paths),
    Avg_Penalty_Paid = mean(colSums(inj_opt * distress_fee))
  ))
}

# ------------------------------------------------------------------------------
# 5. ANALYSIS: FIND THE WINNER
# ------------------------------------------------------------------------------
# Efficiency: Which ratio results in the highest Expected NPV?
# (Usually, there's a peak where the benefit of avoiding the 10% penalty 
# outweighs the cost of the hedge premium).
best_ratio <- opt_results[which.max(opt_results$Expected_NPV), ]

print(opt_results, row.names = FALSE)
cat("\n------------------------------------------------------------------------------\n")
cat(sprintf("OPTIMIZATION RESULT: The most efficient hedge is %s.\n", best_ratio$Hedge_Ratio))
cat(sprintf("This ratio maximizes NPV at €%s by balancing protection vs cost.\n", 
            format(round(best_ratio$Expected_NPV, 0), big.mark=",")))
cat("==============================================================================\n")
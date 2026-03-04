# ==============================================================================
# TASK 4d: HEDGE OPTIMIZATION (THE "SWEET SPOT" FINDER)
# ==============================================================================

# 0. SAFEGUARD: Check for the Unhedged NOCF Matrix
# ------------------------------------------------------------------------------
if (!exists("results_unhedged") || is.null(results_unhedged$monthly_nocf)) {
  stop("FATAL ERROR: 'results_unhedged$monthly_nocf' is missing. Ensure Task 2 is run and saved correctly.")
}

base_nocf    <- results_unhedged$monthly_nocf
hedge_ratios <- seq(0, 1, by = 0.2) # 0%, 20%, 40%, 60%, 80%, 100%
opt_results  <- data.frame()

cat("\n--- STARTING STRESS-TESTED OPTIMIZATION LOOP ---\n")

for (ratio in hedge_ratios) {
  
  # 1. Update the local environment variables
  current_ratio_payoffs <- hedge_base$payoff_matrix * ratio
  current_ratio_premium <- hedge_base$fair_value * ratio
  
  # 2. RUN MINI-WATERFALL 
  A_t_opt      <- matrix(0, nrow = n_months + 1, ncol = n_paths)
  inj_opt      <- matrix(0, nrow = n_months, ncol = n_paths)
  
  for (t in 1:n_months) {
    # Safely combine the base unhedged cash flows with the scaled hedge payoff
    net_cf <- base_nocf[t, ] + current_ratio_payoffs[t, ]
    
    # Cash Account Logic
    pot_bal <- (A_t_opt[t, ] * (1 + r_monthly_rf)) + net_cf
    
    deficits <- pot_bal < 0
    # Only inject capital where there is a deficit
    inj_opt[t, deficits] <- abs(pot_bal[deficits])
    
    # Carry forward the healthy balances
    A_t_opt[t+1, ] <- pmax(pot_bal, 0)
  }
  
  # 3. VALUATION
  payout_60    <- pmax(0, A_t_opt[61, ] - debt_principal)
  pv_payout    <- payout_60 / (1 + r_monthly_eq)^60
  
  # Safely apply discrete discounting to the distress penalties using sweep()
  discount_vec <- 1 / (1 + r_monthly_eq)^(1:n_months)
  penalties_pv_matrix <- sweep(inj_opt * (1 + distress_fee), 1, discount_vec, FUN = "*")
  pv_penalties <- colSums(penalties_pv_matrix)
  
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
best_ratio <- opt_results[which.max(opt_results$Expected_NPV), ]

print(opt_results, row.names = FALSE)
cat("\n------------------------------------------------------------------------------\n")
cat(sprintf("OPTIMIZATION RESULT: The most efficient hedge is %s.\n", best_ratio$Hedge_Ratio))
cat(sprintf("This ratio maximizes NPV at €%s by balancing protection vs cost.\n", 
            format(round(best_ratio$Expected_NPV, 0), big.mark=",")))
cat("==============================================================================\n")
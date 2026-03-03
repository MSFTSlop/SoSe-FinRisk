# ==============================================================================
# TASK 4d: HEDGE OPTIMIZATION (CLEANED DIMENSIONS)
# ==============================================================================

# Ensure we have exactly 60 months of base cash flow (T=1 to T=60)
# This prevents "non-conformable" errors and "out of bounds" errors
base_nocf_forecast <- matrix(base_nocf[2:61, ], nrow = 60, ncol = n_paths)

hedge_ratios <- seq(0, 1, by = 0.25)
opt_results <- data.frame()

cat("\n--- STARTING HEDGE OPTIMIZATION LOOP ---\n")

for (ratio in hedge_ratios) {
  
  # 1. Scale the monthly payoffs by the hedge ratio
  current_payoff <- payoff_matrix * ratio
  
  # 2. Add to forecast base (Both are now 60x10000)
  current_nocf <- base_nocf_forecast + current_payoff
  
  # 3. Calculate NPV (Use current_nocf directly; it is already 60 rows)
  # Ensure discount_factors is length 60
  path_npvs   <- colSums(current_nocf * as.vector(discount_factors[1:60]))
  current_npv <- mean(path_npvs)
  
  # 4. Calculate Cumulative Risk (Life-cycle)
  curr_distress  <- current_nocf < debt_service
  # Identify paths that hit distress at any point up to month 60
  curr_ever_fail <- apply(curr_distress, 2, any) 
  current_risk   <- mean(curr_ever_fail)
  
  # 5. Store Results
  opt_results <- rbind(opt_results, data.frame(
    Hedge_Ratio  = paste0(ratio * 100, "%"),
    Expected_NPV = current_npv,
    Final_Risk   = current_risk
  ))
}

# ------------------------------------------------------------------------------
# 6. EFFICIENCY ANALYSIS
# ------------------------------------------------------------------------------
base_risk <- opt_results$Final_Risk[1]
base_npv  <- opt_results$Expected_NPV[1]

opt_results$Risk_Reduced_pct <- (base_risk - opt_results$Final_Risk) * 100
opt_results$Cost_of_Hedge      <- base_npv - opt_results$Expected_NPV

# Efficiency: Points of risk removed per €1,000 of NPV cost
opt_results$Efficiency_Score <- ifelse(opt_results$Cost_of_Hedge > 0, 
                                       opt_results$Risk_Reduced_pct / (opt_results$Cost_of_Hedge / 1000), 
                                       0)

print(opt_results, row.names = FALSE)
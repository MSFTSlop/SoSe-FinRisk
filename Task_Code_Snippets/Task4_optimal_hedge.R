# ==============================================================================
# TASK 4d: HEDGE OPTIMIZATION (PARTIAL VS. FULL)
# ==============================================================================

# Define the Hedge Ratios to test (0%, 25%, 50%, 75%, 100%)
hedge_ratios <- seq(0, 1, by = 0.25)
opt_results <- data.frame()

cat("\n--- STARTING HEDGE OPTIMIZATION LOOP ---\n")

for (ratio in hedge_ratios) {
  
  # 1. Recalculate Adjusted Payoff (Task 3 Logic)
  # We scale the mwh_volume by the hedge ratio
  current_payoff <- payoff_matrix * ratio
  
  # 2. Recalculate Cash Flows (Task 4 Logic)
  current_nocf <- base_nocf + current_payoff
  
  # 3. Calculate Metrics
  current_npv <- mean(colSums(current_nocf[2:61, ] * discount_factors))
  
  # Calculate Cumulative Risk (Final Month)
  curr_distress  <- current_nocf[2:61, ] < debt_service
  curr_ever_fail <- apply(curr_distress, 2, cumsum) > 0
  current_risk   <- tail(rowMeans(curr_ever_fail), 1)
  
  # 4. Store Results
  opt_results <- rbind(opt_results, data.frame(
    Hedge_Ratio = paste0(ratio * 100, "%"),
    Expected_NPV = current_npv,
    Final_Risk   = current_risk
  ))
}

# ------------------------------------------------------------------------------
# 5. FINAL ANALYSIS DATAFRAME
# ------------------------------------------------------------------------------
# Calculate the 'Efficiency' (How much risk is killed per Euro of NPV lost)
# Formula: (Risk_Reduction) / (NPV_Loss)
base_risk <- opt_results$Final_Risk[1]
base_npv  <- opt_results$Expected_NPV[1]

opt_results$Risk_Reduced_pct <- (base_risk - opt_results$Final_Risk) * 100
opt_results$Cost_of_Hedge     <- base_npv - opt_results$Expected_NPV

# Avoid division by zero for the 0% case
opt_results$Efficiency_Score <- ifelse(opt_results$Cost_of_Hedge > 0, 
                                       opt_results$Risk_Reduced_pct / (opt_results$Cost_of_Hedge / 1e3), 
                                       0)

# ------------------------------------------------------------------------------
# 6. FINAL SUMMARY & CONCLUSION
# ------------------------------------------------------------------------------
cat("\n==============================================================================\n")
cat("FINAL STRATEGIC SUMMARY: HEDGE OPTIMIZATION TABLE\n")
cat("==============================================================================\n")
print(opt_results, row.names = FALSE)
cat("------------------------------------------------------------------------------\n")

# Identify the "Optimal" Ratio (Highest Efficiency Score)
best_ratio_idx <- which.max(opt_results$Efficiency_Score)
best_ratio     <- opt_results$Hedge_Ratio[best_ratio_idx]

cat("CONCLUSION:\n")
cat("- The Unhedged project carries a lifecycle risk of ", round(base_risk * 100, 2), "%.\n")
cat("- The most EFFICIENT hedge level is found at: ", best_ratio, " coverage.\n")

if (opt_results$Final_Risk[best_ratio_idx] < 0.05) {
  cat("- RECOMMENDATION: Implement the ", best_ratio, " hedge. It brings risk below the 5% bank threshold.\n")
} else {
  cat("- RECOMMENDATION: Even with 100% hedge, risk remains high. Consider increasing PPA price or Debt restructuring.\n")
}
cat("==============================================================================\n")
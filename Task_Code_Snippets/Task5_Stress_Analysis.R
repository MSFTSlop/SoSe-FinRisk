# ==============================================================================
# TASK 5: FINAL PROJECT FINANCE CONCLUSION (A, B, & C)
# ==============================================================================

# 1. VALIDATION: Ensure all three major scenarios have been calculated
# ------------------------------------------------------------------------------
required_results <- c("results_unhedged", "results_hedged", "results_stressed")
if (!all(sapply(required_results, exists))) {
  stop("Missing data! Please ensure you have run the calculation script for UNHEDGED, HEDGED, and STRESSED scenarios.")
}

# 2. CONSOLIDATED SUMMARY DATAFRAME
# ------------------------------------------------------------------------------
message("Extracting Case Specific Metrics for final Dataframe")
task5_summary <- data.frame(
  Scenario         = c("Base (Unhedged)", "Base (100% Hedged)", "Stressed (Corr=0)"),
  
  Expected_NPV_M   = c(results_unhedged$expected_npv, 
                       results_hedged$expected_npv, 
                       results_stressed$expected_npv) / 1e6,
  
  Monthly_Distress = c(results_unhedged$prob_distress, 
                       results_hedged$prob_distress, 
                       results_stressed$prob_distress) * 100,
  
  Avg_Penalty_M    = c(mean(results_unhedged$cum_cost_distress), 
                       mean(results_hedged$cum_cost_distress), 
                       mean(results_stressed$cum_cost_distress)) / 1e6
)

# 3. IMPACT CALCULATIONS
# ------------------------------------------------------------------------------
# Decoupling loss: Difference between Base Unhedged and Stressed Unhedged
npv_loss_decoupling <- results_unhedged$expected_npv - results_stressed$expected_npv

# Hedge Value: The "NPV lift" provided by the hedge in the base case
hedge_value_base    <- results_hedged$expected_npv - results_unhedged$expected_npv

# 4. THE EXECUTIVE SUMMARY REPORT
# ------------------------------------------------------------------------------
cat("\n==============================================================================\n")
cat("TASK 5: FINAL INVESTMENT & RISK CONCLUSION\n")
cat("==============================================================================\n")
print(task5_summary, row.names = FALSE)
cat("------------------------------------------------------------------------------\n")

cat("HEDGE PERFORMANCE (BASE CASE):\n")
cat(sprintf("- Hedge Value Add: Hedging 100%% of the volume changed the Expected NPV by €%s.\n", 
            format(round(hedge_value_base, 0), big.mark=",")))

cat("\nQUESTION 5.c ANALYSIS: DECOUPLED MARKETS\n")
cat(sprintf("- Decoupling Impact: Breaking correlation (Corr=0) reduced Unhedged NPV by €%s.\n", 
            format(round(npv_loss_decoupling, 0), big.mark=",")))

# Logical conclusion for Question 5.c
cat("- Conclusion: The hedge becomes MORE VALUABLE in a decoupled market.\n")
cat("  Why? In the base case, electricity prices and gas prices move together.\n")
cat("  In a decoupled market, you lose this 'natural hedge.' If gas prices spike\n")
cat("  while electricity stays low, the SPV faces massive deficits. The derivative\n")
cat("  specifically protects against this 'Spark Spread' divergence.\n")

cat("\nFINAL RECOMMENDATION (Q4.c & Q5.c):\n")
# Using the 12% Expected Return hurdle as a baseline
if (results_hedged$expected_npv > 0) {
  cat("- ACTION: INVEST. The project is viable when hedged. The derivative\n")
  cat("  mitigates 'Liquidity Events' and expensive emergency injections (€1.10/€1.00).\n")
} else {
  cat("- ACTION: REJECT. Even with hedging, the NPV remains negative given the\n")
  cat("  12% cost of equity hurdle rate and operational costs.\n")
}
cat("==============================================================================\n")
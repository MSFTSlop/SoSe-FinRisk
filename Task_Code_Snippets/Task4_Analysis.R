# ==============================================================================
# TASK 4c: SCORECARD (UNHEDGED VS. HEDGED)
# ==============================================================================

# 1. VALIDATION: Check if both scenarios have been run and saved
# ------------------------------------------------------------------------------
if (!exists("results_unhedged") || !exists("results_hedged")) {
  stop("Missing data! Please run your Calculation script for 'UNHEDGED' then 'HEDGED' first.")
}

# 2. EXTRACT METRICS FROM SAVED LISTS
# ------------------------------------------------------------------------------
# Unhedged Metrics
npv_u           <- results_unhedged$expected_npv
prob_u          <- results_unhedged$prob_distress
distress_cost_u <- mean(results_unhedged$cum_cost_distress)

# Hedged Metrics
npv_h           <- results_hedged$expected_npv
prob_h          <- results_hedged$prob_distress
distress_cost_h <- mean(results_hedged$cum_cost_distress)

# 3. CONSTRUCT TASK 4c COMPARISON TABLE
# ------------------------------------------------------------------------------
comparison_df <- data.frame(
  Metric = c(
    "Expected Equity NPV (€)", 
    "Avg. Distress Penalty Cost (€)", 
    "Monthly Prob. of Distress (%)"
  ),
  Unhedged_Base = c(
    round(npv_u, 0), 
    round(distress_cost_u, 0),
    round(prob_u * 100, 2)
  ),
  Hedged_Case = c(
    round(npv_h, 0), 
    round(distress_cost_h, 0),
    round(prob_h * 100, 2)
  ),
  Difference = c(
    round(npv_h - npv_u, 0),
    round(distress_cost_h - distress_cost_u, 0),
    round((prob_h - prob_u) * 100, 2)
  )
)

# 4. ANALYSIS OUTPUTS & Q4c QUALITATIVE PROMPTS
# ------------------------------------------------------------------------------
cat("\n==============================================================================\n")
cat("QUESTION 4c: DOES HEDGING CREATE VALUE?\n")
cat("==============================================================================\n")
# Print the dataframe nicely formatted
print(comparison_df, row.names = FALSE)
cat("------------------------------------------------------------------------------\n")

# Logic for the "Value Add" calculations
npv_delta       <- npv_h - npv_u
penalty_savings <- distress_cost_u - distress_cost_h

cat(sprintf("NPV Impact:       The hedge changes expected investor value by €%s.\n", format(round(npv_delta, 0), big.mark=",")))
cat(sprintf("Distress Impact:  The hedge saves an average of €%s in deadweight penalty fees.\n", format(round(penalty_savings, 0), big.mark=",")))

cat("\n--- GUIDANCE FOR YOUR WRITTEN ANALYSIS ---\n")
cat("1. Should they purchase the derivative? Look at the 'Distress Impact' above vs. the upfront cost.\n")
cat("2. Does it add value? If NPV increases (Difference > 0), yes. Even though the hedge is \n")
cat("   'fairly priced' at risk-free rates, it avoids the 10% deadweight distress penalty.\n")
cat("3. Should they fully hedge? Consider that if wind > 50MW, they are selling excess at \n")
cat("   low spot prices while paying out on the swap. An over-hedge might destroy value.\n")
cat("==============================================================================\n")
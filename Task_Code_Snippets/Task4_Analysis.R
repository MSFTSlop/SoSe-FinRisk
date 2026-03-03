# ==============================================================================
# TASK 4c: SCORECARD (BASE VS. HEDGED ONLY)
# ==============================================================================

# 1. VALIDATION: Only check for Q2 and Q4 variables
# ------------------------------------------------------------------------------
required_vars <- c(
  "expected_npv_unhedged", "cum_prob_distress_unhedged",
  "expected_npv_hedged",   "cum_prob_distress_hedged"
)

if (!all(sapply(required_vars, exists))) {
  stop("Missing data! Please run your Waterfall script for 'UNHEDGED' then 'HEDGED' first.")
}

# 2. CONSTRUCT TASK 4 COMPARISON
# ------------------------------------------------------------------------------
# We use tail(..., 1) to get the final cumulative risk at Month 60
comparison_df <- data.frame(
  Metric = c("Expected Equity NPV (€)", "Life-Cycle Default Risk (%)"),
  
  Unhedged_Base = c(
    round(expected_npv_unhedged, 0), 
    round(tail(cum_prob_distress_unhedged, 1) * 100, 2)
  ),
  
  Hedged_Case = c(
    round(expected_npv_hedged, 0), 
    round(tail(cum_prob_distress_hedged, 1) * 100, 2)
  )
)

# 3. ANALYSIS OUTPUTS
# ------------------------------------------------------------------------------
cat("\n==============================================================================\n")
cat("QUESTION 4c: DOES HEDGING CREATE VALUE?\n")
cat("==============================================================================\n")
print(comparison_df, row.names = FALSE)
cat("------------------------------------------------------------------------------\n")

# Logic for the "Value Add" bullet
npv_delta <- expected_npv_hedged - expected_npv_unhedged
risk_reduction <- tail(cum_prob_distress_unhedged, 1) - tail(cum_prob_distress_hedged, 1)

cat(sprintf("NPV Impact: The hedge adds €%s to investor value.\n", format(round(npv_delta, 0), big.mark=",")))
cat(sprintf("Risk Impact: The hedge removes %s%% of life-cycle default probability.\n", round(risk_reduction * 100, 2)))
cat("==============================================================================\n")
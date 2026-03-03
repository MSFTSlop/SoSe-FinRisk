# ==============================================================================
# TASK 4c: STRATEGIC COMPARISON SCORECARD
# ==============================================================================

# 1. VALIDATION: Ensure all three scenarios have been executed
# ------------------------------------------------------------------------------
required_archived_vars <- c(
  "expected_npv_unhedged", "prob_distress_unhedged", "cum_prob_distress_unhedged",
  "expected_npv_hedged",   "prob_distress_hedged",   "cum_prob_distress_hedged",
  "expected_npv_stressed", "prob_distress_stressed", "cum_prob_distress_stressed"
)

if (!all(sapply(required_archived_vars, exists))) {
  cat("!!! WARNING: Some scenarios are missing. Please run Task 2/4 for:\n")
  cat("1. Pure Unhedged\n2. Hedged\n3. Stressed Unhedged\n")
}

# 2. CONSTRUCT COMPARISON DATAFRAME
# ------------------------------------------------------------------------------
comparison_df <- data.frame(
  Metric = c("Expected NPV (€)", "Monthly Distress Prob.", "Life-Cycle Risk (5yr)"),
  
  Unhedged_Base = c(
    expected_npv_unhedged, 
    prob_distress_unhedged, 
    tail(cum_prob_distress_unhedged, 1)
  ),
  
  Hedged_Case = c(
    expected_npv_hedged, 
    prob_distress_hedged, 
    tail(cum_prob_distress_hedged, 1)
  ),
  
  Stressed_Unhedged = c(
    expected_npv_stressed, 
    prob_distress_stressed, 
    tail(cum_prob_distress_stressed, 1)
  )
)

# 3. QUANTITATIVE ANALYSIS (The "Delta")
# ------------------------------------------------------------------------------
# How much risk does the hedge remove?
hedge_benefit <- comparison_df[3, "Unhedged_Base"] - comparison_df[3, "Hedged_Case"]

# How much does the correlation breakdown hurt us?
stress_impact <- comparison_df[3, "Stressed_Unhedged"] - comparison_df[3, "Unhedged_Base"]

# 4. FINAL REPORTING
# ------------------------------------------------------------------------------
cat("\n==============================================================================\n")
cat("TASK 4c: CONSOLIDATED FINANCIAL RISK ANALYSIS\n")
cat("==============================================================================\n")
print(comparison_df, row.names = FALSE)
cat("------------------------------------------------------------------------------\n")

cat("EXECUTIVE VERDICT:\n")
cat("- HEDGE EFFECTIVENESS: The Spark Spread Option reduces total risk by ", 
    round(hedge_benefit * 100, 2), "% points.\n")

cat("- CORRELATION SENSITIVITY: Decoupling Gas/Elec (Corr=0) increases risk by ", 
    round(stress_impact * 100, 2), "% points.\n")

if (stress_impact > 0.05) {
  cat("\nCRITICAL OBSERVATION: The project relies heavily on the 'Natural Hedge'\n")
  cat("between Gas and Power. If this correlation breaks, the project is unstable.\n")
}

if (comparison_df[3, "Hedged_Case"] < 0.05) {
  cat("\nDECISION: PROCEED WITH HEDGE. It brings risk within bank-acceptable limits (<5%).\n")
} else {
  cat("\nDECISION: CAUTION. Even with a hedge, life-cycle risk remains above 5%.\n")
}
cat("==============================================================================\n")
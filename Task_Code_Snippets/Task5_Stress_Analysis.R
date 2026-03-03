# ==============================================================================
# TASK 5: FINAL PROJECT FINANCE CONCLUSION (A, B, & C)
# ==============================================================================

# --- 5a. CORRELATION ANALYSIS ---
# Theory: We compare the 'Historical' simulation (Task 1) vs 'Shuffled' (Task 3).
corr_impact_npv  <- expected_npv_unhedged - expected_npv_stressed
corr_impact_risk <- prob_distress_stressed - prob_distress_unhedged

# --- 5b. THE NATURAL HEDGE VERDICT ---
# If risk increases significantly under Stress, the 'Natural Hedge' is VITAL.
is_natural_hedge_vital <- corr_impact_risk > 0.02 # Threshold: 2% increase in monthly risk

# --- 5c. CONSOLIDATED SUMMARY DATAFRAME ---
task5_summary <- data.frame(
  Scenario = c("Base (Correlated)", "Hedged (Option)", "Stressed (Corr=0)"),
  Expected_NPV_M = c(expected_npv_unhedged, expected_npv_hedged, expected_npv_stressed) / 1e6,
  Cumulative_Risk = c(tail(cum_prob_distress_unhedged, 1), 
                      tail(cum_prob_distress_hedged, 1), 
                      tail(cum_prob_distress_stressed, 1)) * 100
)

# --- THE EXECUTIVE CAT COMMENTARY ---
cat("\n==============================================================================\n")
cat("TASK 5: FINAL INVESTMENT & RISK CONCLUSION\n")
cat("==============================================================================\n")
print(task5_summary, row.names = FALSE)
cat("------------------------------------------------------------------------------\n")

cat("CRITICAL ANALYSIS (Target 5a & 5b):\n")
cat("- Correlation Sensitivity: Breaking the correlation (Corr=0) resulted in a \n",
    "  risk increase of ", round(corr_impact_risk * 100, 2), "% points.\n")

if (is_natural_hedge_vital) {
  cat("- Natural Hedge Status: VITAL. The project's stability depends on the \n",
      "  historical link between Gas and Power markets.\n")
} else {
  cat("- Natural Hedge Status: MARGINAL. The project is idiosyncratic.\n")
}

cat("\nFINAL INVESTMENT RECOMMENDATION (Target 5c):\n")
if (tail(cum_prob_distress_hedged, 1) < 0.05) {
  cat("- ACTION: INVEST. With the Spark Spread Hedge, the project achieves a \n",
      "  bankable risk profile (<5% cumulative distress).\n")
} else {
  cat("- ACTION: REJECT/RENEGOTIATE. Even with hedging, the project is too risky.\n")
}
cat("==============================================================================\n")
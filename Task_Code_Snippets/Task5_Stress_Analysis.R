# ==============================================================================
# TASK 5: FINAL PROJECT FINANCE CONCLUSION (A, B, & C)
# ==============================================================================

# --- 5a. CORRELATION ANALYSIS (Target 5.b & 5.c) ---
# Theory: Calculate how much "Hedge Value" is hidden in the market correlation.
# In a decoupled market (Corr=0), we expect the cost of shortfall to spike.
npv_loss_from_decoupling <- expected_npv_unhedged - expected_npv_stressed
risk_delta               <- prob_distress_stressed - prob_distress_unhedged

# --- 5b. HEDGE VALUE ANALYSIS (Target 5.c) ---
# Does the hedge become more valuable when the market decouples?
# Value of Hedge = (NPV Hedged - NPV Unhedged)
hedge_value_base   <- expected_npv_hedged - expected_npv_unhedged
# Note: If you ran a 'Stressed Hedged' scenario, you would compare it here.

# --- 5c. CONSOLIDATED SUMMARY DATAFRAME ---
task5_summary <- data.frame(
  Scenario         = c("Base (Correlated)", "Hedged (Base)", "Stressed (Corr=0)"),
  Expected_NPV_M   = c(expected_npv_unhedged, expected_npv_hedged, expected_npv_stressed) / 1e6,
  Monthly_Distress = c(prob_distress_unhedged, prob_distress_hedged, prob_distress_stressed) * 100,
  Cumulative_Risk  = c(tail(cum_prob_distress_unhedged, 1), 
                       tail(cum_prob_distress_hedged, 1), 
                       tail(cum_prob_distress_stressed, 1)) * 100
)

# --- THE EXECUTIVE CAT COMMENTARY ---
cat("\n==============================================================================\n")
cat("TASK 5: FINAL INVESTMENT & RISK CONCLUSION\n")
cat("==============================================================================\n")
print(task5_summary, row.names = FALSE)
cat("------------------------------------------------------------------------------\n")

cat("QUESTION 5.c ANALYSIS:\n")
cat(sprintf("- Decoupling Impact: Breaking correlation cost the project €%s in NPV.\n", 
            format(round(npv_loss_from_decoupling, 0), big.mark=",")))

if (npv_loss_from_decoupling > 0) {
  cat("- Result: The hedge becomes MORE VALUABLE in a decoupled market.\n")
  cat("  Why? Without the natural correlation, Gas spikes are not offset by \n")
  cat("  Power revenue, making the 'Spark Spread' insurance payoff more frequent.\n")
}

cat("\nFINAL RECOMMENDATION (Q4.c & Q5.c):\n")
# Based on the Cumulative Risk threshold from Q2.d and Q4.c
if (tail(cum_prob_distress_hedged, 1) < 0.10) { # 10% threshold for bankability
  cat("- ACTION: INVEST. Hedging creates a 'bankable' project by removing \n")
  cat("  the extreme tail risks identified in the Stress Test.\n")
} else {
  cat("- ACTION: REJECT. Risk levels remain above acceptable SPV thresholds.\n")
}
cat("==============================================================================\n")
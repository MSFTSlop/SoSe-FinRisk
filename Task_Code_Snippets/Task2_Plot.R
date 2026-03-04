# ==============================================================================
# TASK 2: VISUALIZATION OF VALUATION & LIFE-CYCLE RISK
# ==============================================================================

# Set up a side-by-side plotting area
par(mfrow = c(1, 2), mar = c(5, 4, 4, 1))

# ------------------------------------------------------------------------------
# PLOT 1 (Q2b & Q4c): NPV DISTRIBUTION (10,000 PATHS)
# ------------------------------------------------------------------------------
# Plotting the Net Present Value in Millions of Euros
hist(path_npvs / 1e6, breaks = 50, col = "skyblue", border = "white",
     main = paste("Project Equity NPV\n(", mode_label, ")", sep=""),
     xlab = "NPV (Millions EUR)", ylab = "Frequency")

# Add a vertical line for the Expected NPV (Average)
abline(v = expected_npv / 1e6, col = "darkblue", lwd = 3)
text(expected_npv / 1e6, par("usr")[4] * 0.9, 
     labels = paste("Mean: €", round(expected_npv/1e6, 2), "M"), 
     pos = 4, col = "darkblue", font = 2)

# Add dashed lines for the 95% Confidence Intervals
abline(v = npv_95_bounds[1] / 1e6, col = "darkred", lwd = 2, lty = 2)
abline(v = npv_95_bounds[2] / 1e6, col = "darkgreen", lwd = 2, lty = 2)

# ------------------------------------------------------------------------------
# PLOT 2 (Q2d): DISTRIBUTION OF CUMULATIVE COST OF DISTRESS
# ------------------------------------------------------------------------------
# We filter out paths with ZERO distress costs to see the actual distribution 
# of the penalty ONLY when it occurs.
distress_only_paths <- cumulative_cost_of_distress[cumulative_cost_of_distress > 0]

if (length(distress_only_paths) > 0) {
  # Plotting the 10% distress penalty in Millions of Euros
  hist(distress_only_paths / 1e6, breaks = 50, col = "firebrick", border = "white",
       main = paste("Cumulative Cost of Distress\n(Penalty Distribution)"),
       xlab = "Distress Cost (Millions EUR)", ylab = "Frequency")
  
  # Add a vertical line for the Average Distress Cost (among paths that had distress)
  avg_distress <- mean(distress_only_paths)
  abline(v = avg_distress / 1e6, col = "black", lwd = 3, lty = 2)
  text(avg_distress / 1e6, par("usr")[4] * 0.8, 
       labels = paste("Mean (if distressed):\n€", round(avg_distress/1e6, 2), "M"), 
       pos = 4, col = "black", font = 2)
  
} else {
  # Fallback if the scenario had absolutely zero distress across all 10,000 paths
  plot(1, type = "n", xlab = "", ylab = "", main = "Cumulative Cost of Distress", axes = FALSE)
  text(1, 1, "No Distress Events Recorded", col = "darkgreen", cex = 1.5, font = 2)
}

# Reset plotting parameters
par(mfrow = c(1, 1))
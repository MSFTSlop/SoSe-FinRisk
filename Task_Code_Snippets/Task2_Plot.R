# ==============================================================================
# TASK 2: VISUALIZATION OF VALUATION & LIFE-CYCLE RISK
# ==============================================================================

# Set up a side-by-side plotting area
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

# ------------------------------------------------------------------------------
# PLOT 2b: NPV DISTRIBUTION (10,000 PATHS)
# ------------------------------------------------------------------------------
hist(path_npv / 1e6, breaks = 50, col = "skyblue", border = "white",
     main = "2b: Expected Project NPV",
     xlab = "NPV (Millions EUR)", ylab = "Frequency")

# Add a vertical line for the Expected NPV (Average)
abline(v = expected_npv / 1e6, col = "darkblue", lwd = 3)
text(expected_npv / 1e6, par("usr")[4]*0.8, 
     labels = paste("Mean: €", round(expected_npv/1e6, 1), "M"), 
     pos = 4, col = "darkblue", font = 2)

# ------------------------------------------------------------------------------
# PLOT 2d: CUMULATIVE PROBABILITY OF DISTRESS
# ------------------------------------------------------------------------------
# 'cum_prob_distress' is the vector of 60 probabilities calculated in 2d
plot(1:n_months, cum_prob_distress, type = "l", lwd = 3, col = "firebrick",
     main = "2d: Life-Cycle Distress Risk",
     xlab = "Month", ylab = "Cumulative Probability",
     ylim = c(0, max(cum_prob_distress) * 1.2))

# Highlight the final risk level
points(n_months, tail(cum_prob_distress, 1), pch = 19, col = "firebrick")
text(n_months, tail(cum_prob_distress, 1), 
     labels = paste(round(tail(cum_prob_distress, 1)*100, 1), "%"), 
     pos = 2, col = "firebrick", font = 2)

grid(col = "gray80", lty = "dotted")

# Reset plotting parameters
par(mfrow = c(1, 1))
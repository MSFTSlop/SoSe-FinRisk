# ==============================================================================
# TASK 1: STATISTICAL DIAGNOSIS (CORRECTED)
# ==============================================================================

## so fat tail is testing essentially if we have tails on our distribution. 
## if the kurt_val is bigger 0.5 we may have fat tails. if its smaller we 
## assume a normal distribution. same with the pvalue box test. if its smaller 
## 0.05 we assume that high risk clusters together while pvlaue bigger 0.05 is 
## randomness. we check if we ahve randomness in the p value  (smaller 0.05) or 
## the curt val is bigger than one (fat tails). in our case nothing applies so we can use linear

# 1. Extract and Clean Data
ret_elec <- na.omit(returns_df$Electricity)

# 2. Calculate Diagnostic KPIs
mu_elec    <- mean(ret_elec)
sigma_elec <- sd(ret_elec)

# KPI: Excess Kurtosis (using moments package or manual calculation)
# We ensure the variable name is consistent: kurt_val
kurt_val   <- moments::kurtosis(ret_elec) - 3

# KPI: ARCH Effect (Ljung-Box Test on squared returns)
# We calculate this specifically to use in the cat() report
arch_test  <- Box.test(ret_elec^2, lag = 12, type = "Ljung-Box")

# KPI: Volatility Correlation (Lag-1)
sq_ret_lag  <- ret_elec[1:(length(ret_elec)-1)]^2
sq_ret_curr <- ret_elec[2:length(ret_elec)]^2
vol_corr    <- cor(sq_ret_curr, sq_ret_lag)

# 3. Diagnostic Summary Table
diagnosis_results <- data.frame(
  Metric = c("Mean Return", "Avg Volatility (Sigma)", "Excess Kurtosis", "Vol Correlation (Lag-1)"),
  Value  = c(mu_elec, sigma_elec, kurt_val, vol_corr),
  Interpretation = c(
    "Expected monthly drift",
    "Standard risk level",
    "High value = Higher risk of Default/Distress",
    "> 0.1 indicates ARCH/GARCH is necessary"
  )
)

print(diagnosis_results)

# 4. Visual Diagnosis
if(!is.null(dev.list())) dev.off() 
par(mfrow = c(2, 1), mar = c(3, 4, 2, 1), oma = c(0, 0, 2, 0))

# Plot 1: Standard Returns
plot(ret_elec, type = "l", col = "darkblue", lwd = 1,
     main = "Time Series of Electricity Returns", ylab = "Return (%)", xlab = "")
abline(h = mu_elec, col = "orange", lty = 2, lwd = 2)

# Plot 2: Squared Returns
plot(ret_elec^2, type = "l", col = "darkred", lwd = 1,
     main = "Squared Returns (Volatility Proxy)", ylab = "Sq Return", xlab = "Months")
abline(h = mean(ret_elec^2), col = "gray", lty = 3)

mtext("Statistical Diagnostic: Linear vs. GARCH Suitability", outer = TRUE, cex = 1.2, font = 2)

# 5. Interpretation Report (Using Corrected Variable Names)
cat("\n", rep("=", 50), "\n")
cat("TASK 1: STATISTICAL DIAGNOSTIC REPORT\n")
cat(rep("=", 50), "\n\n")

cat("1. MEAN & DRIFT:\n")
cat("   - Average Monthly Return:", round(mu_elec * 100, 4), "%\n")
cat("   - Interpretation: Baseline drift for simulation.\n\n")

cat("2. TAIL RISK (Kurtosis):\n")
cat("   - Excess Kurtosis:", round(kurt_val, 4), "\n")
cat("   - Interpretation:", ifelse(kurt_val > 0.5, 
                                   "FAT TAILS detected. Extreme shocks (Task 5 Distress) are likely.", 
                                   "Normal tails. Linear assumptions are safer."), "\n\n")

cat("3. VOLATILITY CLUSTERING (ARCH-Effect):\n")
cat("   - Ljung-Box p-value:", round(arch_test$p.value, 6), "\n")
cat("   - Interpretation:", ifelse(arch_test$p.value < 0.05,
                                   "SIGNIFICANT CLUSTERING. Use GARCH(1,1).",
                                   "No clustering. Linear/Random Walk is sufficient."), "\n\n")

cat("4. MODEL SELECTION VERDICT:\n")
# Choosing model based on p-value and Kurtosis
final_model <- ifelse(arch_test$p.value < 0.05 | kurt_val > 1, "GARCH(1,1)", "Linear/Random Walk")
cat("   - Primary Model: ", final_model, "\n")
cat("   - Reasoning: Capturing volatility spikes is vital for Task 4 & 5 accuracy.\n")
cat(rep("=", 50), "\n")

# ==============================================================================
# TASK 1: STATISTICAL REASONING & MODEL SELECTION LOGIC
# ==============================================================================

# 1. MEAN & DRIFT REASONING:
# -------------------------
# The mean (mu) represents the 'deterministic' part of the price path.
# - LIMITATION: In a Random Walk, we assume prices 'drift' at a constant rate.
# - PROJECT IMPACT: If the mean is positive, the project looks profitable on 
#   average, but the mean tells us nothing about the 'sequence of returns'. 
#   In an SPV, a high mean won't save you if you hit a cluster of bad months 
#   early on (Task 5 Distress).

# 2. EXCESS KURTOSIS (THE "FAT TAIL" LIMIT):
# ------------------------------------------
# - THRESHOLD: Excess Kurtosis > 0 indicates 'Leptokurtic' behavior.
# - LINEAR LIMIT: A Linear/Random Walk model assumes a Normal Distribution.
#   In a normal curve, extreme events (shocks > 3 standard deviations) are 
#   statistically 'impossible'.
# - GARCH ADVANTAGE: High kurtosis implies that 'Black Swan' price spikes occur 
#   more often than the bell curve predicts. GARCH allows us to model these 
#   shocks, ensuring our Task 5 cash flow projections aren't 'too optimistic'.

# 3. P-VALUE & ARCH EFFECTS (THE "MEMORY" TEST):
# ----------------------------------------------
# We use the Ljung-Box test on squared returns to check for autocorrelation.
# - P-VALUE > 0.05: Implies 'White Noise'. Volatility is random. Today's shock 
#   does not change tomorrow's risk. Linear models (Random Walk) are sufficient.
# - P-VALUE < 0.05: Implies 'Volatility Clustering'. High-risk periods tend to 
#   cluster together in waves.
# - WHY GARCH: A Random Walk is 'memoryless'. GARCH captures 'Risk Persistence'.
#   For our SPV, if Gas prices spike in Month 10, GARCH ensures they stay high 
#   for Month 11 and 12, testing the project's liquidity (Task 5) more rigorously.

# 4. FINAL VERDICT RATIONALE:
# ---------------------------
# We choose GARCH(1,1) over Linear/Random Walk if (p-value < 0.05) OR (Kurtosis > 1).
# This ensures that our Monte Carlo paths reflect the 'volatile streaks' 
# common in energy markets, rather than just independent monthly 'vibrations'.
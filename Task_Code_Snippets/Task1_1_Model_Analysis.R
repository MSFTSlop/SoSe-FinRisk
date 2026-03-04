# ==============================================================================
# TASK 1: STATISTICAL DIAGNOSIS (STABILITY PATCH)
# ==============================================================================

# 1. ENSURE DATA IS ACCESSIBLE
# Replace 'returns_df' with whatever your actual data object is named
target_vars <- c("Electricity", "NaturalGas", "Carbon", "WindUtilization")

# Initialize results
diagnosis_results <- data.frame()
garch_votes       <- 0 

cat("\n", rep("=", 60), "\n")
cat("TASK 1: MULTI-VARIABLE STATISTICAL DIAGNOSTIC REPORT\n")
cat(rep("=", 60), "\n\n")

message("Analyzing every variable to find the optimal Simulation Model")
# 2. THE STABILIZED DIAGNOSTIC LOOP
for (var in target_vars) {
  
  # SAFETY CHECK: Does the column exist?
  if (!(var %in% names(returns_df))) {
    warning(paste("Column", var, "not found in data. Skipping."))
    next
  }
  
  # THE FIX: Force coercion to a numeric vector [[var]] can sometimes return a tibble
  # as.numeric(as.matrix(...)) is the 'nuclear option' to ensure it is a simple vector
  raw_data <- returns_df[[var]]
  ret_var  <- as.numeric(na.omit(raw_data))
  
  # Check if we actually have enough data points to run a test
  if (length(ret_var) < 20) {
    warning(paste("Variable", var, "has too few observations."))
    next
  }
  
  # 3. CALCULATE KPIs
  mu_val    <- mean(ret_var)
  sigma_val <- sd(ret_var)
  kurt_val  <- moments::kurtosis(ret_var) - 3
  
  # ARCH Effect (Ljung-Box Test on squared returns)
  # Ensure we use at least a few lags
  arch_test <- Box.test(ret_var^2, lag = 12, type = "Ljung-Box")
  p_val     <- arch_test$p.value
  
  # Logical Triggers
  has_fat_tails  <- kurt_val > 0
  has_clustering <- p_val < 0.05
  needs_garch    <- has_fat_tails | has_clustering
  
  if (needs_garch) garch_votes <- garch_votes + 1
  
  # 4. APPEND RESULTS
  # Using stringsAsFactors=FALSE to keep it stable
  new_row <- data.frame(
    Variable        = var,
    Mean_Return     = round(mu_val, 4),
    Volatility      = round(sigma_val, 4),
    Excess_Kurtosis = round(kurt_val, 4),
    ARCH_p_value    = round(p_val, 6),
    Fat_Tails       = ifelse(has_fat_tails, "YES (>0)", "NO"),
    Vol_Clustering  = ifelse(has_clustering, "YES (<0.05)", "NO"),
    stringsAsFactors = FALSE
  )
  diagnosis_results <- rbind(diagnosis_results, new_row)
}

message("Simulation Done; Compiling Results")
# Print the final consolidated table
print(diagnosis_results)

# 5. VISUAL DIAGNOSIS
# Clear any stuck graphics devices before starting
while(!is.null(dev.list())) dev.off() 
par(mfrow = c(2, 2), mar = c(3, 4, 3, 1), oma = c(0, 0, 2, 0))

colors <- c("Electricity" = "darkblue", "NaturalGas" = "darkred", "Carbon" = "darkgreen", "WindUtilization" = "purple")

for (var in diagnosis_results$Variable) {
  # Re-extract for plotting
  ret_var <- as.numeric(na.omit(returns_df[[var]]))
  plot(ret_var^2, type = "l", col = colors[var], 
       main = paste(var, "(Sq. Returns)"), ylab = "Sq Return", xlab = "")
}
mtext("Volatility Clustering Check", outer = TRUE, cex = 1.2, font = 2)



# ==============================================================================
# TASK 1: FINAL MODEL SELECTION VERDICT
# ==============================================================================

cat("\n", rep("-", 60), "\n")
cat("FINAL INTEGRATED MODEL VERDICT & TRADE-OFF ANALYSIS\n")
cat(rep("-", 60), "\n")

# Logic: Count how many variables triggered a 'GARCH' requirement
garch_triggers <- sum(diagnosis_results$Fat_Tails == "YES (>0)" | 
                        diagnosis_results$Vol_Clustering == "YES (<0.05)")

cat(sprintf("- Total Variables Analyzed: %d\n", nrow(diagnosis_results)))
cat(sprintf("- Variables with High-Risk Signatures: %d\n", garch_triggers))

# --- THE DECISION ENGINE ---
if (garch_triggers > 0) {
  cat("\n>>> AI ANALYSIS: MIXED SIGNALS DETECTED IN DATA <<<\n\n")
  cat(paste("The data shows that", garch_triggers, "out of", nrow(diagnosis_results), 
            "variables exhibit high-risk signatures (ARCH effects or Fat Tails).\n"))
  cat("Because the dataset contains conflicting statistical profiles, you have two valid modeling options:\n\n")
  
  cat("[OPTION 1] GARCH(1,1) / NON-LINEAR SIMULATION (Data-Driven Recommendation)\n")
  cat("PROS: \n")
  cat(" - Captures 'Volatility Persistence' for the variables that triggered the high-risk flags.\n")
  cat(" - Because Electricity, Gas, and Carbon are coupled in the Waterfall loop, modeling the\n")
  cat("   'memory' of shocks in fuel costs prevents a biased 'Spark Spread' calculation.\n")
  cat("CONS: \n")
  cat(" - Introduces mathematical complexity to variables that may not strictly require it.\n\n")
  
  cat("[OPTION 2] LINEAR / GEOMETRIC BROWNIAN MOTION\n")
  cat("PROS: \n")
  cat(" - Computationally efficient, simpler to audit, and perfectly valid for the variables\n")
  cat("   that showed normal distributions and memoryless returns (p-values > 0.05).\n")
  cat("CONS: \n")
  cat(" - Structurally ignores the identified high-risk variables, which may lead to an\n")
  cat("   underestimation of Liquidity Distress (Task 5) during severe market shocks.\n\n")
  
  cat("CONCLUSION: The AI recommends GARCH(1,1) to safely capture the identified tail risks,\n")
  cat("but the Linear framework remains a viable alternative if model parsimony is preferred.\n")
  
} else {
  cat("\n>>> AI ANALYSIS: UNIFORM DATA PROFILE DETECTED <<<\n\n")
  
  cat("[OPTION] LINEAR / GEOMETRIC BROWNIAN MOTION (Strong Recommendation)\n")
  cat("REASONING:\n")
  cat("1. All analyzed variables exhibit Excess Kurtosis <= 0 and p-values > 0.05.\n")
  cat("2. Returns appear to be normally distributed and memoryless across the entire dataset.\n")
  cat("3. A simpler Linear model is strictly sufficient and avoids unnecessary complexity.\n\n")
  
  cat("CONCLUSION: The AI strongly recommends Geometric Brownian Motion. Using GARCH(1,1)\n")
  cat("on this specific dataset would result in over-parameterization with no added benefit.\n")
}



cat(rep("=", 60), "\n")
cat("Please proceed to the main script to make your final selection.\n")
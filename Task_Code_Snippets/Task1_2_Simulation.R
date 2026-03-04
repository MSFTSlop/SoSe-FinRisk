# ==============================================================================
# TASK 1.2: MONTE CARLO SIMULATION ENGINE (10,000 PATHS)
# ==============================================================================
# ------------------------------------------------------------------------------
# ARCHITECTURAL DECISION & MODEL SELECTION RATIONALE:
# ------------------------------------------------------------------------------
#
# 1. GEOMETRIC BROWNIAN MOTION (GBM) - For Electricity, Gas, and Carbon Prices:
#    - WHY: We use a multiplicative (Geometric) model because commodity prices 
#      are physically bounded at zero. A standard additive model could produce 
#      impossible negative prices. GBM ensures prices stay positive and scales 
#      volatility as a percentage of the price.
#    - REJECTED ALTERNATIVES: We rejected 'Mean Reversion' (Ornstein-Uhlenbeck) 
#      because our tests showed no evidence that energy prices "pull back" to 
#      a fixed mean within the 5-year project horizon (Task 5).
#
# 2. ADDITIVE RANDOM WALK (LOGIT-SPACE) - For Wind Utilization:
#    - WHY: Wind utilization is strictly bounded [0, 1]. To model this, we 
#      transform historical % into 'Logit-Space' (unbounded). We apply an 
#      additive random walk here to allow symmetric shocks, then use the 
#      Sigmoid transform to map values back to the physical 0-100% range.
#    - REJECTED ALTERNATIVES: We rejected standard GBM for Wind because GBM 
#      only prevents prices from hitting zero; it does not prevent Wind 
#      Utilization from exceeding 100%, which would violate physics.
#
# 3. LINEAR VS. GARCH VERDICT:
#    - WHY: Since Task 1.1 yielded a p-value > 0.05 (no clustering) and 
#      Kurtosis < 0.5 (no fat tails), the complexity of GARCH or Jump-Diffusion 
#      is not mathematically justified. The Random Walk is the most 
#      parsimonious and scientifically robust choice for this dataset.
# ------------------------------------------------------------------------------
# ==============================================================================
# TASK 1.2: CORRELATED MULTIVARIATE SIMULATION ENGINE (10,000 PATHS)
# ==============================================================================

# ------------------------------------------------------------------------------
# ARCHITECTURAL REWORK: A MULTIVARIATE MATRIX APPROACH vs univariabble approach
# ------------------------------------------------------------------------------
# 1. THE CORRELATION FIX (Cholesky Decomposition):
#    - PROBLEM: Independent rnorm() calls created 0% correlation between Gas/Elec.
#    - SOLUTION: We now calculate the historical Correlation Matrix and apply 
#      Cholesky Decomposition. By multiplying independent shocks by the Cholesky 
#      L-matrix, we force the simulated paths to respect historical relationships.
    
    ## if we didnt do that and calculated for each individual variable the simulation
    ## we got a correllation close to zero what wouldnt make much sense
    
    ## for reaso s as to why we still used GBM and the additive random walk see above

# 2. JENSEN'S INEQUALITY (CONVEXITY) CORRECTION
# ------------------------------------------------------------------------------
# PRINCIPLE: Volatility Drag Adjustment
# We apply the term (-0.5 * sigma^2) to the drift of GBM assets (Elec, Gas, Carbon).
#
# THEORY: 
# In a stochastic process, the expected value of the exponential of a random 
# variable is greater than the exponential of its expected value: E[exp(X)] > exp(E[X]).
# Without this adjustment, the 'Mean' of our 10,000 simulated price paths would 
# drift higher than the 'Arithmetic Mean' calculated from our historical data.
#
# PROJECT IMPACT: 
# This correction cancels out the 'Convexity Bias.' It ensures that the average 
# simulated revenue matches our target expectations, preventing an overestimation 
# of project value and an underestimation of Default Risk.


# ==============================================================================
# TASK 1.2: MULTIVARIATE MONTE CARLO ENGINE (CORRELATED RANDOM WALKS)
# ==============================================================================
# SUMMARY OF OPERATIONS:
# 1. CALIBRATION: Extracts drift (mu) and volatility (sigma) from historical data.
# 2. COVARIANCE: Uses Cholesky Decomposition to link variables (e.g., Gas & Elec).
# 3. EVOLUTION:
#    - Prices (Elec, Gas, Carb): Evolved via Geometric Brownian Motion (GBM).
#    - Wind (Utilization): Evolved via Additive Random Walk in Logit-Space.
# 4. TRANSFORMATION: Maps Logit paths back to physical [0,1] range (Sigmoid).
# 5. PRICING: Dynamically sets PPA price at 20% discount to simulated mean.
# ==============================================================================
# ==============================================================================
# TASK 1.2: MONTE CARLO SIMULATION ENGINE (10,000 PATHS)
# ==============================================================================
# ------------------------------------------------------------------------------
# ARCHITECTURAL DECISION & MODEL SELECTION RATIONALE:
# 1. GEOMETRIC BROWNIAN MOTION (GBM) - For Electricity, Gas, and Carbon Prices.
# 2. ADDITIVE RANDOM WALK (LOGIT-SPACE) - For Wind Utilization bounded [0, 1].
# 3. CHOLEKSY DECOMPOSITION - To link the variables mathematically.
# ==============================================================================

# 1. CALIBRATION & CORRELATION MATRIX
# ------------------------------------------------------------------------------
vars <- c("Electricity", "NaturalGas", "Carbon", "WindUtilization")

mu    <- sapply(returns_df[, vars], mean, na.rm = TRUE)
sigma <- sapply(returns_df[, vars], sd, na.rm = TRUE)

cor_mat <- cor(returns_df[, vars], use = "pairwise.complete.obs")

current_mode_label <- if (exists("stress_test_mode") && stress_test_mode == TRUE) "STRESS" else "BASE"

if (current_mode_label == "STRESS") {
  L <- diag(n_vars) 
  cat("STRESS TEST ACTIVE: L-Matrix is now Identity (Correlation = 0.0)\n")
} else {
  L <- t(chol(cor_mat))
  cat("BASE CASE ACTIVE: L-Matrix derived from historical correlations\n")
}

# ==============================================================================
# 2. STOCHASTIC ENGINE PRE-ALLOCATION
# ==============================================================================
n_paths  <- 10000 
n_months <- 60    # 5-year project horizon
n_vars   <- length(vars)

sim_list <- list(
  Electricity     = matrix(NA, n_months + 1, n_paths),
  NaturalGas      = matrix(NA, n_months + 1, n_paths),
  Carbon          = matrix(NA, n_months + 1, n_paths),
  WindUtilization = matrix(NA, n_months + 1, n_paths)
)

sim_list$Electricity[1, ]     <- p_elec_0 
sim_list$NaturalGas[1, ]      <- p_gas_0  
sim_list$Carbon[1, ]          <- p_carb_0 
sim_list$WindUtilization[1, ] <- l_wind_0 

message("Blueprint done; starting Monte Carlo simulation...")

# ==============================================================================
# 3. THE MULTIVARIATE EVOLUTION LOOP
# ==============================================================================
set.seed(123) 

for (t in 2:(n_months + 1)) {
  
  Z_indep <- matrix(rnorm(n_paths * n_vars), ncol = n_vars)
  Z_corr  <- Z_indep %*% t(L) 
  
  for (v in 1:n_vars) {
    var_name <- vars[v]
    
    if (var_name == "WindUtilization") {
      sim_list[[var_name]][t, ] <- sim_list[[var_name]][t-1, ] + mu[v] + sigma[v] * Z_corr[, v]
    } else {
      drift <- (mu[v] - 0.5 * sigma[v]^2)
      sim_list[[var_name]][t, ] <- sim_list[[var_name]][t-1, ] * exp(drift + sigma[v] * Z_corr[, v])
    }
  }
}

# ==============================================================================
# 4. FINAL EXTRACTION & BACK-TRANSFORMATION
# ==============================================================================
sim_elec  <- sim_list$Electricity
sim_gas   <- sim_list$NaturalGas
sim_carb  <- sim_list$Carbon
sim_logit <- sim_list$WindUtilization

sim_util  <- exp(sim_logit) / (1 + exp(sim_logit))

actual_sim_corr <- cor(as.vector(sim_elec[2:61,]), as.vector(sim_gas[2:61,]))
cat("Audit: Realized Elec/Gas Correlation is", round(actual_sim_corr, 4), "\n")

# ==============================================================================
# 5. DYNAMIC PPA PRICING 
# ==============================================================================
if (current_mode_label == "STRESS") {
  if (!exists("p_ppa_locked")) {
    cat("!!! WARNING: Stress Test run before Base Case. Using Historical Mean for PPA !!!\n")
    p_ppa_locked <- mean(returns_df$Electricity, na.rm = TRUE) * 0.80
  }
  p_ppa <- p_ppa_locked
  cat("STRESS TEST: Retaining PPA Price from Base Case (€", round(p_ppa, 2), ")\n")
} else {
  p_ppa        <- mean(sim_elec[2:61, ]) * 0.80
  p_ppa_locked <- p_ppa 
  cat("BASE CASE: PPA Price established at €", round(p_ppa, 2), "\n")
}

# ==============================================================================
# 6. FINAL ANNOTATION & PLOTTING (Restored)
# ==============================================================================
# 
q_elec <- apply(sim_elec, 1, quantile, probs = c(0.1, 0.5, 0.9))

# Dynamic Title based on the regime
plot_title <- sprintf("Monte Carlo: %s Electricity Paths", 
                      ifelse(current_mode_label == "STRESS", "Decoupled", "Correlated"))

matplot(0:n_months, sim_elec, type = "l", lty = 1, 
        col = rgb(0.1, 0.3, 0.6, 0.01), 
        main = plot_title,
        xlab = "Month", ylab = "Price (EUR/MWh)")

mtext(sprintf("Density of 10,000 paths; PPA set at €%.2f (20%% discount)", p_ppa), 
      side = 3, line = 0.5, cex = 0.9)

lines(0:n_months, q_elec[1, ], col = "red",    lwd = 2, lty = 2) 
lines(0:n_months, q_elec[2, ], col = "yellow", lwd = 3)          
lines(0:n_months, q_elec[3, ], col = "green",  lwd = 2, lty = 2) 

legend("topleft", legend = c("P90", "P50 (Median)", "P10"),
       col = c("green", "yellow", "red"), lwd = 3, bg="white")

# ==============================================================================
# 7. UNIVERSE ARCHIVING (HAND-OFF TO TASKS 2 & 3)
# ==============================================================================
current_universe <- list(
  sim_elec = sim_elec,
  sim_gas  = sim_gas,
  sim_carb = sim_carb,
  sim_util = sim_util,
  p_ppa    = p_ppa
)

if (current_mode_label == "STRESS") {
  sim_universe_stressed <- current_universe
  cat(">>> SIMULATION SECURED: 'sim_universe_stressed' is ready for Task 2/3.\n")
} else {
  sim_universe_base <- current_universe
  cat(">>> SIMULATION SECURED: 'sim_universe_base' is ready for Task 2/3.\n")
}
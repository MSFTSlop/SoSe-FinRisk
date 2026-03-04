# ==============================================================================
# TASK 1.2: MONTE CARLO SIMULATION ENGINE (10,000 PATHS)
# ==============================================================================
# ------------------------------------------------------------------------------
# ARCHITECTURAL DECISION & MODEL SELECTION RATIONALE (LINEAR / GBM):
# ------------------------------------------------------------------------------
#
# 1. MULTIVARIATE GEOMETRIC BROWNIAN MOTION (GBM) - For Elec, Gas, and Carbon:
#    - WHY: Assumes constant volatility and normal distribution of returns. 
#      This is the industry standard for baseline valuation when ARCH effects
#      are deemed immaterial or for achieving model parsimony.
#
# 2. ADDITIVE RANDOM WALK (LOGIT-SPACE) - For Wind Utilization:
#    - Maps physical percentage [0, 1] into unbounded Logit-Space to safely 
#      apply stochastic shocks, converting back via Sigmoid function at the end.
#
# 3. GAUSSIAN COPULA (CHOLESKY DECOMPOSITION):
#    - WHY: Uses the standard historical correlation matrix to link variables. 
#      Shocks are drawn from a Multivariate Normal distribution rather than a 
#      Fat-Tailed Student-t mixture.
# ------------------------------------------------------------------------------

vars   <- c("Electricity", "NaturalGas", "Carbon", "WindUtilization")
n_vars <- length(vars) 

# ==============================================================================
# 1. GBM PARAMETER ESTIMATION (STATIC DRIFT & VOLATILITY)
# ==============================================================================
message("Estimating GBM Parameters (Historical Mean and Volatility)")
gbm_params <- list()

cat("Calibrating Linear parameters...\n")
for (v in vars) {
  series <- as.numeric(na.omit(returns_df[[v]]))
  
  # For GBM, we only need the static historical mean and variance/std dev
  mu_hist  <- mean(series)
  var_hist <- var(series)
  sig_hist <- sqrt(var_hist)
  
  gbm_params[[v]] <- list(
    mu    = mu_hist,
    sigma = sig_hist,
    var   = var_hist
  )
}

# ==============================================================================
# 2. CALIBRATION & CORRELATION MATRIX (GAUSSIAN COPULA)
# ==============================================================================
message("Building the Cholesky Decomposition Correlation Matrix")

# For standard GBM, we calculate correlation directly on historical returns
cor_mat <- cor(returns_df[vars], use = "pairwise.complete.obs")

current_mode_label <- if (exists("stress_test_mode") && stress_test_mode == TRUE) "STRESS" else "BASE"

if (current_mode_label == "STRESS") {
  L <- diag(n_vars) 
  cat("STRESS TEST ACTIVE: L-Matrix is now Identity (Correlation = 0.0)\n")
} else {
  L <- t(chol(cor_mat))
  cat("BASE CASE ACTIVE: L-Matrix derived from historical returns\n")
}

# ==============================================================================
# 3. STOCHASTIC ENGINE PRE-ALLOCATION
# ==============================================================================
message("Pre Allocating Variables")
n_paths  <- 10000 
n_months <- 60    

sim_list <- list(
  Electricity     = matrix(NA, n_months + 1, n_paths),
  NaturalGas      = matrix(NA, n_months + 1, n_paths),
  Carbon          = matrix(NA, n_months + 1, n_paths),
  WindUtilization = matrix(NA, n_months + 1, n_paths)
)

l_wind_0 <- log(u_wind_0 / (1 - u_wind_0)) 

sim_list$Electricity[1, ]     <- p_elec_0 
sim_list$NaturalGas[1, ]      <- p_gas_0  
sim_list$Carbon[1, ]          <- p_carb_0 
sim_list$WindUtilization[1, ] <- l_wind_0 

message("GBM blueprint complete; igniting Monte Carlo engine...")

# ==============================================================================
# 4. THE MULTIVARIATE GBM EVOLUTION LOOP (GAUSSIAN COPULA)
# ==============================================================================
set.seed(42) 

message("Starting Monte Carlo Simulation")

for (t in 1:n_months) {
  
  # 1. Draw independent Standard Normals
  Z_indep <- matrix(rnorm(n_paths * n_vars), ncol = n_vars)
  
  # 2. Correlate the normals using your Cholesky Matrix (Gaussian Copula)
  Z_corr <- Z_indep %*% t(L)
  
  for (i in 1:n_vars) {
    var_name <- vars[i]
    p <- gbm_params[[var_name]]
    
    # Static Shock Scaling (No GARCH Volatility Clustering)
    eps_shock <- p$sigma * Z_corr[, i]
    
    # Price Evolution
    if (var_name == "WindUtilization") {
      # Standard additive random walk in logit space
      sim_list[[var_name]][t+1, ] <- sim_list[[var_name]][t, ] + p$mu + eps_shock
    } else {
      # Jensen's Inequality Correction applied to static variance
      drift <- p$mu - 0.5 * p$var
      sim_list[[var_name]][t+1, ] <- sim_list[[var_name]][t, ] * exp(drift + eps_shock)
    }
  }
}

message("Monte Carlo Simulation completed successfully")

# ==============================================================================
# 5. FINAL EXTRACTION & BACK-TRANSFORMATION
# ==============================================================================
sim_elec  <- sim_list$Electricity
sim_gas   <- sim_list$NaturalGas
sim_carb  <- sim_list$Carbon
sim_logit <- sim_list$WindUtilization

sim_util  <- exp(sim_logit) / (1 + exp(sim_logit))

actual_sim_corr <- cor(as.vector(sim_elec[2:61,]), as.vector(sim_gas[2:61,]))
cat("Audit: Realized Elec/Gas Correlation is", round(actual_sim_corr, 4), "\n")

# ==============================================================================
# 6. DYNAMIC PPA PRICING (LOCKED FOR HEDGE STABILITY)
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
# 7. FINAL ANNOTATION & PLOTTING 
# ==============================================================================
message("Plot the Simulation")

q_elec <- apply(sim_elec, 1, quantile, probs = c(0.1, 0.5, 0.9))

plot_title <- sprintf("Linear GBM Gaussian-Copula MC: %s Electricity Paths", 
                      ifelse(current_mode_label == "STRESS", "Decoupled", "Correlated"))

matplot(0:n_months, sim_elec, type = "l", lty = 1, 
        col = rgb(0.1, 0.3, 0.6, 0.01), 
        ylim = c(0, 3000), 
        main = plot_title, 
        xlab = "Month", ylab = "Price (EUR/MWh)")

mtext(sprintf("Density of 10,000 paths; PPA locked at €%.2f (20%% discount)", p_ppa), 
      side = 3, line = 0.5, cex = 0.9)

lines(0:n_months, q_elec[1, ], col = "red",    lwd = 2, lty = 2) 
lines(0:n_months, q_elec[2, ], col = "yellow", lwd = 3)          
lines(0:n_months, q_elec[3, ], col = "green",  lwd = 2, lty = 2) 

legend("topleft", legend = c("P90", "P50 (Median)", "P10"),
       col = c("green", "yellow", "red"), lwd = 3, bg="white")

# ==============================================================================
# 8. UNIVERSE ARCHIVING (HAND-OFF TO TASKS 2 & 3)
# ==============================================================================
message("Saving Case specific Variables")
current_universe <- list(
  sim_elec = sim_elec,
  sim_gas  = sim_gas,
  sim_carb = sim_carb,
  sim_util = sim_util,
  p_ppa    = p_ppa
)

if (current_mode_label == "STRESS") {
  sim_universe_stressed <- current_universe
  cat(">>> SIMULATION SECURED: 'sim_universe_stressed' (Gaussian Copula) is ready.\n")
} else {
  sim_universe_base <- current_universe
  cat(">>> SIMULATION SECURED: 'sim_universe_base' (Gaussian Copula) is ready.\n")
}
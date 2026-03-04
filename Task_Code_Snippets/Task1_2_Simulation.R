# ==============================================================================
# TASK 1.2: MONTE CARLO SIMULATION ENGINE (10,000 PATHS)
# ==============================================================================
# ------------------------------------------------------------------------------
# ARCHITECTURAL DECISION & MODEL SELECTION RATIONALE (UPGRADED TO GARCH):
# ------------------------------------------------------------------------------
#
# 1. MULTIVARIATE GARCH(1,1) - For Electricity, Gas, and Carbon Prices:
#    - WHY: Task 1.1 statistical diagnostics revealed Excess Kurtosis (>0) and 
#      significant Ljung-Box p-values (<0.05). This proves that energy markets 
#      exhibit "Volatility Clustering" (memory) and "Fat Tails" (extreme shocks).
#    - REJECTED ALTERNATIVE: Geometric Brownian Motion (GBM) was rejected because 
#      it assumes constant, independent volatility. Using GBM would dangerously 
#      underestimate the SPV's exposure to consecutive high-risk months.
#
# 2. ADDITIVE RANDOM WALK (LOGIT-SPACE) - For Wind Utilization:
#    - THE LOGIT INITIALIZATION BUG & RATIONALE:
#      Wind utilization is a physical percentage strictly bounded between [0, 1].
#      If we feed a raw percentage (e.g., 0.35) directly into an unbounded random 
#      walk, the simulation could push the value below 0 or above 1, violating physics.
#      To fix this, we map the initial starting utilization (u_wind_0) into an 
#      unbounded "Logit-Space" (from -Inf to +Inf) using: L_0 = ln(U_0 / (1 - U_0)). 
#      The GARCH shocks happen safely in this unbounded space, and we use the 
#      Sigmoid function at the end to map it back to a strict [0, 1] range.
#
# 3. CHOLESKY DECOMPOSITION (CORRELATION MATRIX):
#    - WHY: Independent variables would yield a 0.0 correlation. By multiplying 
#      independent standard normal shocks by the Cholesky L-matrix, we force 
#      simulated paths to respect historical relationships (e.g., Gas driving Power).
# ------------------------------------------------------------------------------
# ==============================================================================
# 1. CALIBRATION & CORRELATION MATRIX
# ==============================================================================
message("Building the Cholesky Decomposition Correlation Matrix")

vars   <- c("Electricity", "NaturalGas", "Carbon", "WindUtilization")
# BUG FIX: Define n_vars before it is called by the Identity matrix logic
n_vars <- length(vars) 

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
# 2. GARCH(1,1) PARAMETER ESTIMATION
# ==============================================================================
message("Estimating GARCH(1,1) Parameters")
garch_params <- list()
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(0, 0), include.mean = TRUE))

cat("Calibrating GARCH(1,1) parameters...\n")
for (v in vars) {
  series <- as.numeric(na.omit(returns_df[[v]]))
  
  # Try fitting GARCH; use a fallback if the historical series is perfectly flat/problematic
  fit <- tryCatch({ ugarchfit(spec = spec, data = series, solver = "hybrid") }, 
                  error = function(e) NULL, warning = function(w) NULL)
  
  if (!is.null(fit) && convergence(fit) == 0) {
    coefs <- coef(fit)
    garch_params[[v]] <- list(
      mu = coefs["mu"], omega = coefs["omega"], alpha = coefs["alpha1"], beta = coefs["beta1"],
      h_inf = coefs["omega"] / (1 - coefs["alpha1"] - coefs["beta1"])
    )
  } else {
    cat("Warning: GARCH fit failed for", v, "- falling back to historical estimates.\n")
    var_hist <- var(series)
    garch_params[[v]] <- list(
      mu = mean(series), omega = var_hist * 0.05, alpha = 0.10, beta = 0.85, h_inf = var_hist
    )
  }
}

# ==============================================================================
# 3. STOCHASTIC ENGINE PRE-ALLOCATION
# ==============================================================================
message("Pre Allocating Variables")
n_paths  <- 10000 
n_months <- 60    # 5-year project horizon

sim_list <- list(
  Electricity     = matrix(NA, n_months + 1, n_paths),
  NaturalGas      = matrix(NA, n_months + 1, n_paths),
  Carbon          = matrix(NA, n_months + 1, n_paths),
  WindUtilization = matrix(NA, n_months + 1, n_paths)
)

h_list   <- list() # To track conditional variance
eps_list <- list() # To track conditional shocks
for (v in vars) {
  h_list[[v]]   <- matrix(NA, n_months, n_paths)
  eps_list[[v]] <- matrix(NA, n_months, n_paths)
}

# BUG FIX: Safely map physical Wind percentage to Logit-Space BEFORE simulating
# Assuming u_wind_0 is provided in the environment as a physical percentage (e.g., 0.35)
l_wind_0 <- log(u_wind_0 / (1 - u_wind_0)) 

sim_list$Electricity[1, ]     <- p_elec_0 
sim_list$NaturalGas[1, ]      <- p_gas_0  
sim_list$Carbon[1, ]          <- p_carb_0 
sim_list$WindUtilization[1, ] <- l_wind_0 

message("GARCH blueprint complete; igniting Monte Carlo engine...")

# ==============================================================================
# 4. THE MULTIVARIATE GARCH EVOLUTION LOOP
# ==============================================================================
set.seed(42) 

message("Starting Monte Carlo Simulation")

for (t in 1:n_months) {
  
  # Generate Correlated Standard Normal Shocks
  Z_indep <- matrix(rnorm(n_paths * n_vars), ncol = n_vars)
  Z_corr  <- Z_indep %*% t(L) 
  
  for (i in 1:n_vars) {
    var_name <- vars[i]
    p <- garch_params[[var_name]]
    
    # Variance Update (Volatility Clustering Engine)
    if (t == 1) {
      h_list[[var_name]][t, ] <- p$h_inf
    } else {
      h_list[[var_name]][t, ] <- p$omega + (p$alpha * eps_list[[var_name]][t-1, ]^2) + (p$beta * h_list[[var_name]][t-1, ])
    }
    
    # Shock Scaling
    eps_list[[var_name]][t, ] <- sqrt(h_list[[var_name]][t, ]) * Z_corr[, i]
    
    # Price Evolution
    if (var_name == "WindUtilization") {
      # Unbounded Logit walk
      sim_list[[var_name]][t+1, ] <- sim_list[[var_name]][t, ] + p$mu + eps_list[[var_name]][t, ]
    } else {
      # Geometric model with DYNAMIC Jensen's Inequality correction (-0.5 * h_t)
      drift <- p$mu - 0.5 * h_list[[var_name]][t, ]
      sim_list[[var_name]][t+1, ] <- sim_list[[var_name]][t, ] * exp(drift + eps_list[[var_name]][t, ])
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

# Map Logit back to bounded [0, 1] physical utilization
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

plot_title <- sprintf("GARCH(1,1) Monte Carlo: %s Electricity Paths", 
                      ifelse(current_mode_label == "STRESS", "Decoupled", "Correlated"))

matplot(0:n_months, sim_elec, type = "l", lty = 1, 
        col = rgb(0.1, 0.3, 0.6, 0.01), 
        ylim = c(0, 3000),  # Truncates view to €3000/MWh for readabilit
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
  cat(">>> SIMULATION SECURED: 'sim_universe_stressed' (GARCH) is ready for Task 2/3.\n")
} else {
  sim_universe_base <- current_universe
  cat(">>> SIMULATION SECURED: 'sim_universe_base' (GARCH) is ready for Task 2/3.\n")
}
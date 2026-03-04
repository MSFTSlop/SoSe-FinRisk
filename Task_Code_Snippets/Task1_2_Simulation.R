# ==============================================================================
# TASK 1.2: MONTE CARLO SIMULATION ENGINE (10,000 PATHS)
# ==============================================================================
# ------------------------------------------------------------------------------
# ARCHITECTURAL DECISION & MODEL SELECTION RATIONALE (UPGRADED TO GARCH):
# ------------------------------------------------------------------------------
#
# 1. MULTIVARIATE GARCH(1,1) (STUDENT-T) - For Elec, Gas, and Carbon Prices:
#    - WHY: Task 1.1 statistical diagnostics revealed Excess Kurtosis (>0). 
#      This model uses distribution.model = "std" to explicitly model "Fat Tails" 
#      and extreme market shocks, replacing the standard normal distribution.
#
# 2. ADDITIVE RANDOM WALK (LOGIT-SPACE) - For Wind Utilization:
#    - Maps physical percentage [0, 1] into unbounded Logit-Space to safely 
#      apply stochastic shocks, converting back via Sigmoid function at the end.
#
# 3. STUDENT-T COPULA (CHOLESKY DECOMPOSITION & CHI-SQUARE MIXTURE):
#    - WHY: To capture true structural dependency and joint tail risks. We 
#      calculate the correlation matrix on the GARCH Standardized Residuals, 
#      correlate standard normals, and divide by a shared Chi-Square variable.
# ------------------------------------------------------------------------------

vars   <- c("Electricity", "NaturalGas", "Carbon", "WindUtilization")
n_vars <- length(vars) 

# ==============================================================================
# 1. GARCH(1,1) PARAMETER ESTIMATION & RESIDUAL EXTRACTION
# ==============================================================================
message("Estimating GARCH(1,1) Parameters and Extracting Residuals")
garch_params <- list()

# ### CRITICAL FIX 1: FAKE FAT TAILS ###
spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                   mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
                   distribution.model = "std") 

std_res_df <- data.frame(matrix(NA, nrow = nrow(returns_df), ncol = n_vars))
colnames(std_res_df) <- vars

cat("Calibrating GARCH(1,1) parameters...\n")
for (v in vars) {
  series <- as.numeric(na.omit(returns_df[[v]]))
  valid_indices <- !is.na(returns_df[[v]])
  
  fit <- tryCatch({ ugarchfit(spec = spec, data = series, solver = "hybrid") }, 
                  error = function(e) NULL, warning = function(w) NULL)
  
  if (!is.null(fit) && convergence(fit) == 0) {
    coefs <- coef(fit)
    std_res_df[[v]][valid_indices] <- as.numeric(residuals(fit, standardize = TRUE))
    
    garch_params[[v]] <- list(
      mu = coefs["mu"], omega = coefs["omega"], alpha = coefs["alpha1"], beta = coefs["beta1"],
      # ### CRITICAL FIX 2: STARTING VARIANCE EXPLOSION ###
      last_var = tail(sigma(fit), 1)^2, 
      shape = coefs["shape"] 
    )
  } else {
    cat("Warning: GARCH fit failed for", v, "- falling back to historical estimates.\n")
    var_hist <- var(series)
    std_res_df[[v]][valid_indices] <- (series - mean(series)) / sqrt(var_hist)
    
    garch_params[[v]] <- list(
      mu = mean(series), omega = var_hist * 0.05, alpha = 0.10, beta = 0.85, 
      last_var = var_hist,
      shape = 5 
    )
  }
}

# ==============================================================================
# 2. CALIBRATION & CORRELATION MATRIX (THE COPULA)
# ==============================================================================
message("Building the Cholesky Decomposition Correlation Matrix")

# ### CRITICAL FIX 3: COPULA MATH (STANDARDIZED RESIDUALS) ###
cor_mat <- cor(std_res_df, use = "pairwise.complete.obs")

current_mode_label <- if (exists("stress_test_mode") && stress_test_mode == TRUE) "STRESS" else "BASE"

if (current_mode_label == "STRESS") {
  L <- diag(n_vars) 
  cat("STRESS TEST ACTIVE: L-Matrix is now Identity (Correlation = 0.0)\n")
} else {
  L <- t(chol(cor_mat))
  cat("BASE CASE ACTIVE: L-Matrix derived from GARCH standardized residuals\n")
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

h_list   <- list() 
eps_list <- list() 
for (v in vars) {
  h_list[[v]]   <- matrix(NA, n_months, n_paths)
  eps_list[[v]] <- matrix(NA, n_months, n_paths)
}

l_wind_0 <- log(u_wind_0 / (1 - u_wind_0)) 

sim_list$Electricity[1, ]     <- p_elec_0 
sim_list$NaturalGas[1, ]      <- p_gas_0  
sim_list$Carbon[1, ]          <- p_carb_0 
sim_list$WindUtilization[1, ] <- l_wind_0 

nu <- mean(sapply(vars, function(v) garch_params[[v]]$shape))
if (nu <= 2) nu <- 5 

message("GARCH blueprint complete; igniting Monte Carlo engine...")

# ==============================================================================
# 4. THE MULTIVARIATE GARCH EVOLUTION LOOP (TRUE STUDENT-T COPULA)
# ==============================================================================
set.seed(42) 

message("Starting Monte Carlo Simulation")

for (t in 1:n_months) {
  
  # ### FINAL FIX: TRUE STUDENT-T COPULA VIA CHI-SQUARE MIXTURE ###
  # 1. Draw independent Standard Normals
  Z_indep <- matrix(rnorm(n_paths * n_vars), ncol = n_vars)
  
  # 2. Correlate the normals using your Cholesky Matrix
  Z_corr_norm <- Z_indep %*% t(L)
  
  # 3. Draw the SHARED Chi-Square mixing variable (One value per path)
  W <- rchisq(n_paths, df = nu)
  
  # 4. Create true Multivariate Student-t shocks by scaling the normals
  Z_t_corr <- Z_corr_norm / sqrt(W / nu)
  
  # 5. Standardize back to unit variance for the GARCH engine
  Z_corr <- Z_t_corr * sqrt((nu - 2) / nu)
  
  for (i in 1:n_vars) {
    var_name <- vars[i]
    p <- garch_params[[var_name]]
    
    # Variance Update (Volatility Clustering Engine)
    if (t == 1) {
      h_list[[var_name]][t, ] <- p$last_var
    } else {
      h_list[[var_name]][t, ] <- p$omega + (p$alpha * eps_list[[var_name]][t-1, ]^2) + (p$beta * h_list[[var_name]][t-1, ])
    }
    
    # Shock Scaling
    eps_list[[var_name]][t, ] <- sqrt(h_list[[var_name]][t, ]) * Z_corr[, i]
    
    # Price Evolution
    if (var_name == "WindUtilization") {
      sim_list[[var_name]][t+1, ] <- sim_list[[var_name]][t, ] + p$mu + eps_list[[var_name]][t, ]
    } else {
      # Jensen's Inequality Correction (Keeps NPV grounded in reality!)
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

plot_title <- sprintf("GARCH(1,1) t-Copula Monte Carlo: %s Electricity Paths", 
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
  cat(">>> SIMULATION SECURED: 'sim_universe_stressed' (True t-Copula) is ready.\n")
} else {
  sim_universe_base <- current_universe
  cat(">>> SIMULATION SECURED: 'sim_universe_base' (True t-Copula) is ready.\n")
}
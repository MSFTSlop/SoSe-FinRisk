# ==============================================================================
# TASK 1.2: MONTE CARLO SIMULATION ENGINE (10,000 PATHS)
# ==============================================================================

# ------------------------------------------------------------------------------
# ARCHITECTURAL DECISION & MODEL SELECTION RATIONALE:
# ------------------------------------------------------------------------------
# Based on the Statistical Diagnosis in Task 1.1, we have implemented a 
# dual-model Random Walk framework. Below is the justification for selecting 
# these specific models over more complex alternatives:
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

# 1. SIMULATION PARAMETERS
set.seed(123)       # Ensures reproducibility for audit trails
n_paths  <- 10000   # 10k paths to capture extreme 'tail' risks in Task 5
n_months <- 60      # 5-year project duration
vars     <- c("Electricity", "NaturalGas", "Carbon", "WindUtilization")

# 2. PARAMETER CALIBRATION
# Extracting monthly drift (mu) and volatility (sigma) from historical returns
calib <- sapply(returns_df[, vars], function(x) {
  c(mu = mean(x, na.rm = TRUE), sigma = sd(x, na.rm = TRUE))
})

# 3. CORE SIMULATION FUNCTION
# This engine generates the "10,000 Universes" required for project valuation.
run_simulation <- function(start_val, mu, sigma, n_p, n_m, is_logit = FALSE) {
  # Pre-allocate matrix for performance (Rows = Months, Cols = Paths)
  out <- matrix(NA, nrow = n_m + 1, ncol = n_p)
  out[1, ] <- start_val  # Setting the T=0 Anchor from historical data
  
  for (p in 1:n_p) {
    # Generating 60 independent monthly shocks (Z-scores)
    z <- rnorm(n_m)
    
    if (is_logit) {
      # ADDITIVE MODEL: Captures symmetric fluctuations in the logit scale
      shocks <- mu + (sigma * z)
      out[2:(n_m + 1), p] <- start_val + cumsum(shocks)
    } else {
      # GBM MODEL: Multiplicative growth with Jensen's Inequality correction
      # (mu - 0.5 * sigma^2) ensures the simulated mean matches the data mean.
      drift <- (mu - 0.5 * sigma^2)
      growth <- exp(cumsum(drift + (sigma * z)))
      out[2:(n_m + 1), p] <- start_val * growth
    }
  }
  return(out)
}

# 4. GENERATING THE DATA MATRICES
sim_elec  <- run_simulation(p_elec_0, calib["mu","Electricity"], calib["sigma","Electricity"], n_paths, n_months)
sim_gas   <- run_simulation(p_gas_0,  calib["mu","NaturalGas"],  calib["sigma","NaturalGas"],  n_paths, n_months)
sim_carb  <- run_simulation(p_carb_0, calib["mu","Carbon"],      calib["sigma","Carbon"],      n_paths, n_months)
sim_logit <- run_simulation(l_wind_0, calib["mu","WindUtilization"], calib["sigma","WindUtilization"], n_paths, n_months, is_logit = TRUE)

# 5. BACK-TRANSFORMATION (SIGMOID)
# Converts Logit-Space paths back into physical Wind Utilization % [0, 1]
sim_util  <- exp(sim_logit) / (1 + exp(sim_logit))

# ------------------------------------------------------------------------------
# OUTPUT: We now have 4 matrices (61 rows x 10,000 columns) ready for 
# Task 2 (Revenue), Task 3 (Cost), and the Task 5 Distress Analysis.
# ------------------------------------------------------------------------------

# ==============================================================================
# TASK 1.2: VISUALIZING THE 10,000 PATH "UNIVERSE" (ELECTRICITY)
# ==============================================================================

# 1. CALCULATE STATISTICAL QUANTILE BOUNDARIES
# We compress 10,000 paths into 3 key lines: P10 (Risk), P50 (Median), P90 (Upside)
# 'sim_elec' is the 61x10000 matrix from your simulation
q_elec <- apply(sim_elec, 1, quantile, probs = c(0.1, 0.5, 0.9))

# 2. GENERATE THE FAN CHART
# We use 'rgb' with a very low alpha (0.015) to create a density "cloud"
# This allows us to see all 10,000 paths simultaneously
matplot(0:n_months, sim_elec, type = "l", lty = 1, 
        col = rgb(0.1, 0.3, 0.6, 0.015), # Extremely transparent blue
        main = "Monte Carlo: 10,000 Electricity Price Paths",
        xlab = "Month", ylab = "Price (EUR/MWh)",
        xaxt = "n") # Custom axis for months

# 3. OVERLAY THE PROJECT RISK BOUNDARIES
# These provide the "navigation" through the cloud
lines(0:n_months, q_elec[1, ], col = "red",    lwd = 3, lty = 2) # 10th Percentile (Stress)
lines(0:n_months, q_elec[2, ], col = "yellow", lwd = 4)         # 50th Percentile (Expected)
lines(0:n_months, q_elec[3, ], col = "green",  lwd = 3, lty = 2) # 90th Percentile (Upside)

# 4. CUSTOMIZE AXIS AND LEGEND
axis(1, at = seq(0, n_months, by = 12)) # Yearly tick marks
legend("topleft", 
       legend = c("90th Percentile (Upside)", "50th Percentile (Median)", "10th Percentile (Stress)"),
       col = c("green", "yellow", "red"), lwd = 4, lty = c(2, 1, 2), 
       bg = "white", cex = 0.8)

# 5. TASK-SPECIFIC ANNOTATION
# This text explains the relevance to the future project phases
mtext("Density of 10,000 paths based on Random Walk / GBM calibration", side = 3, line = 0.5, cex = 0.9)
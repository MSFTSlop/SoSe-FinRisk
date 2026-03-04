# QUESTION 1 MARKET SIMULATION AND CALIBRATION
# MONTE CARLO WITH MULTIVARIATE NORMAL


# Install packages (if you have not)
install.packages("readxl",  dependencies=TRUE)
install.packages("mvtnorm", dependencies=TRUE)

# Load libraries
library(readxl)
library(mvtnorm)


# ==============================================================================
# STEP 1 — LOAD DATA
# ==============================================================================

# Set your working directory (change to your path)
#setwd("C:/Users/Bruker/OneDrive/Desktop/GRA 6513 Financial Risk Management")

# Load Excel sheets
returns_data   <- read_excel("Assignment_1_data.xlsx", sheet="Returns")
logit_wind_data <- read_excel("Assignment_1_data.xlsx", sheet="Logit_Wind")
last_price_data <- read_excel("Assignment_1_data.xlsx", sheet="Last_Price")

# Remove Date column from returns
returns        <- returns_data[, -1]
returns_matrix <- as.matrix(returns)


# ==============================================================================
# STEP 2 — CALIBRATE MODEL PARAMETERS
# ==============================================================================

# Mean vector and covariance matrix from historical returns
mu    <- colMeans(returns_matrix)
Sigma <- cov(returns_matrix)

# Correlation matrix (for reporting)
cor_matrix <- cor(returns_matrix)

cat("Mean vector:\n");         print(mu)
cat("\nCovariance matrix:\n"); print(Sigma)
cat("\nCorrelation matrix:\n");print(cor_matrix)


# ==============================================================================
# STEP 3 — SET INITIAL VALUES
# ==============================================================================

# FIX: Use actual last observed prices from Last_Price sheet (not index = 1)
#      This is necessary for Q2 so that cash flows are in real EUR/MWh terms.
elec0   <- as.numeric(last_price_data[1, "Electricity"])    # 135.02 EUR/MWh
gas0    <- as.numeric(last_price_data[1, "NaturalGas"])     # 75.705 EUR/MWh
carbon0 <- as.numeric(last_price_data[1, "Carbon"])         # 79.80  EUR/ton

# FIX: Wind initial value is the last observed LOGIT (not raw utilization).
#      The Returns sheet column "WindUtilization" contains changes in logit(U),
#      so we must track the logit level and update additively.
wind_logit0 <- tail(logit_wind_data$WindUtilization, 1)     # -0.4942 (logit scale)

cat("\nInitial prices:\n")
cat("  Electricity :", elec0,      "EUR/MWh\n")
cat("  Natural Gas :", gas0,       "EUR/MWh\n")
cat("  Carbon      :", carbon0,    "EUR/ton\n")
cat("  Wind logit0 :", wind_logit0,"(logit scale)\n")
cat("  Wind util0  :", exp(wind_logit0)/(1+exp(wind_logit0)), "\n")


# ==============================================================================
# STEP 4 — SIMULATION SETTINGS
# ==============================================================================

n_sim    <- 10000
n_months <- 60
n_assets <- 4   # Electricity, WindUtilization (logit changes), NaturalGas, Carbon

set.seed(42)    # Reproducibility


# ==============================================================================
# STEP 5 — MONTE CARLO SIMULATION OF RETURNS
# ==============================================================================

# Draw all joint innovations at once (faster than looping over simulations)
# sim_returns[t, asset, sim]
sim_returns <- array(0, dim=c(n_months, n_assets, n_sim))

for(i in 1:n_sim)
{
  sim_returns[,,i] <- rmvnorm(
    n     = n_months,
    mean  = mu,
    sigma = Sigma
  )
}


# ==============================================================================
# STEP 6 — BUILD PRICE PATHS AND WIND LOGIT PATH
#
#   (CRITICAL): Wind utilization must be updated ADDITIVELY on the logit
#   scale, not multiplicatively.  The data column "WindUtilization" in the
#   Returns sheet contains Δlogit(U_t), i.e. logit(U_t) - logit(U_{t-1}).
#   Therefore:
#       logit(U_t) = logit(U_{t-1}) + Δlogit_t        <-- ADDITIVE
#   while prices follow:
#       P_t        = P_{t-1} * exp(r_t)               <-- MULTIPLICATIVE
# ==============================================================================

# Price paths for Electricity (col 1), NaturalGas (col 3), Carbon (col 4)
# dim: [n_months+1, 3, n_sim]  —  columns mapped: 1=Elec, 2=Gas, 3=Carbon
price_cols      <- c(1, 3, 4)          # positions in returns_matrix
initial_prices  <- c(elec0, gas0, carbon0)
n_prices        <- length(price_cols)

sim_prices <- array(0, dim=c(n_months+1, n_prices, n_sim))

for(i in 1:n_sim)
{
  sim_prices[1,,i] <- initial_prices
  
  for(t in 2:(n_months+1))
  {
    sim_prices[t,,i] <- sim_prices[t-1,,i] *
      exp(sim_returns[t-1, price_cols, i])
  }
}

# Wind logit path (col 2 in returns — additive delta on logit scale)
sim_wind_logit <- matrix(0, nrow=n_months+1, ncol=n_sim)

for(i in 1:n_sim)
{
  sim_wind_logit[1, i] <- wind_logit0
  
  for(t in 2:(n_months+1))
  {
    sim_wind_logit[t, i] <- sim_wind_logit[t-1, i] +   # <-- ADDITIVE
      sim_returns[t-1, 2, i]
  }
}

# Convert logit back to utilization: U = e^L / (1 + e^L)
wind_utilization <- exp(sim_wind_logit) / (1 + exp(sim_wind_logit))


# ==============================================================================
# STEP 7 — VALIDATION
# ==============================================================================

cat("\n====================================\n")
cat("VALIDATION RESULTS\n")
cat("====================================\n")

# Dimensions
cat("\nsim_prices dimensions   :", dim(sim_prices),      "\n")
cat("sim_wind_logit dimensions:", dim(sim_wind_logit),   "\n")
cat("wind_utilization dims    :", dim(wind_utilization), "\n")

# Starting values (should match initial prices exactly)
cat("\nStarting prices check (sim 1):\n")
cat("  Electricity:", sim_prices[1, 1, 1], "(expected:", elec0,   ")\n")
cat("  NaturalGas :", sim_prices[1, 2, 1], "(expected:", gas0,    ")\n")
cat("  Carbon     :", sim_prices[1, 3, 1], "(expected:", carbon0, ")\n")
cat("  Wind logit :", sim_wind_logit[1,1], "(expected:", wind_logit0,")\n")
cat("  Wind util  :", wind_utilization[1,1],
    "(expected:", exp(wind_logit0)/(1+exp(wind_logit0)), ")\n")

# Returns moment check
sim_returns_matrix <- do.call(
  rbind,
  lapply(1:n_sim, function(i) sim_returns[,,i])
)

cat("\nHistorical means:\n");  print(colMeans(returns_matrix))
cat("\nSimulated means:\n");   print(colMeans(sim_returns_matrix))
cat("\nHistorical covariance:\n"); print(cov(returns_matrix))
cat("\nSimulated covariance:\n");  print(cov(sim_returns_matrix))

# Wind bounds — must always be in (0, 1)
cat("\nWind utilization range:\n")
print(range(wind_utilization))


# ==============================================================================
# STEP 8 — SUMMARY STATISTICS (PER TIME STEP)
#
# NOTE ON MEANS VALIDATION: sim_returns are drawn from rmvnorm(mu, Sigma),
#   completely independently of initial prices.  So changing initial prices
#   from index=1 to real EUR values does NOT affect return moments at all.
#   Small discrepancies between historical and simulated means (e.g. 0.001 vs
#   0.00075 for Electricity) are pure Monte Carlo noise.  With 600,000 draws
#   the standard error of the mean is ~0.000117, so a gap of ~0.00025 is only
#   ~2 SE — well within expected sampling variation, not a bug.
# ==============================================================================

months <- 0:n_months   # x-axis: month 0 (starting) through month 60

# Helper: per-time-step quantile bands — returns matrix [n_months+1 x 4]
row_quantile <- function(mat, probs) t(apply(mat, 1, quantile, probs=probs))

elec_mean   <- apply(sim_prices[,1,],  1, mean)
gas_mean    <- apply(sim_prices[,2,],  1, mean)
carbon_mean <- apply(sim_prices[,3,],  1, mean)
wind_mean   <- apply(wind_utilization, 1, mean)

elec_q   <- row_quantile(sim_prices[,1,],  c(0.05, 0.25, 0.75, 0.95))
gas_q    <- row_quantile(sim_prices[,2,],  c(0.05, 0.25, 0.75, 0.95))
carbon_q <- row_quantile(sim_prices[,3,],  c(0.05, 0.25, 0.75, 0.95))
wind_q   <- row_quantile(wind_utilization, c(0.05, 0.25, 0.75, 0.95))


# ==============================================================================
# STEP 9 — FAN CHART HELPER
#
# Replaces matplot() of all 10,000 paths, which produces a solid colored blob
# and is very slow to render.  Instead we draw:
#   (1) Shaded 90% band (5th-95th pct) per time step  — lightest fill
#   (2) Shaded 50% band (25th-75th pct) per time step — darker fill
#   (3) 100 randomly sampled paths                    — for texture
#   (4) Mean path in red
# ==============================================================================

fan_chart <- function(months, sample_mat, mean_line, q_bands,
                      main, ylab, col_dark, col_light, n_sample=100)
{
  ylo <- min(q_bands[,1]) * 0.95
  yhi <- max(q_bands[,4]) * 1.05
  
  plot(months, mean_line, type="n", ylim=c(ylo, yhi),
       main=main, xlab="Month", ylab=ylab,
       cex.main=1.1, font.main=2)
  
  # 90% band
  polygon(c(months, rev(months)),
          c(q_bands[,1], rev(q_bands[,4])),
          col=col_light, border=NA)
  
  # 50% band
  polygon(c(months, rev(months)),
          c(q_bands[,2], rev(q_bands[,3])),
          col=col_dark, border=NA)
  
  # Sample paths for texture
  for(j in sample(ncol(sample_mat), n_sample))
    lines(months, sample_mat[,j], col=rgb(0,0,0,0.07), lwd=0.4)
  
  # Mean
  lines(months, mean_line, col="red", lwd=2.5)
  
  legend("topleft",
         legend=c("Mean", "50% band", "90% band"),
         col   =c("red", col_dark, col_light),
         lwd=c(2.5, 8, 8), lty=1, bty="n", cex=0.85)
}


# ==============================================================================
# STEP 10 — PLOTS
# ==============================================================================

par(mfrow=c(2,2), mar=c(4,4,3,1))

fan_chart(months, sim_prices[,1,], elec_mean,   elec_q,
          main="Monte Carlo: Electricity Price", ylab="EUR / MWh",
          col_dark=rgb(0.12,0.47,0.71,0.55), col_light=rgb(0.12,0.47,0.71,0.20))

fan_chart(months, wind_utilization, wind_mean,  wind_q,
          main="Monte Carlo: Wind Utilization",  ylab="Utilization [0, 1]",
          col_dark=rgb(0.50,0.00,0.50,0.55), col_light=rgb(0.50,0.00,0.50,0.20))

fan_chart(months, sim_prices[,2,], gas_mean,    gas_q,
          main="Monte Carlo: Natural Gas Price", ylab="EUR / MWh",
          col_dark=rgb(0.84,0.15,0.16,0.55), col_light=rgb(0.84,0.15,0.16,0.20))

fan_chart(months, sim_prices[,3,], carbon_mean, carbon_q,
          main="Monte Carlo: Carbon Price",      ylab="EUR / ton",
          col_dark=rgb(0.17,0.63,0.17,0.55), col_light=rgb(0.17,0.63,0.17,0.20))

par(mfrow=c(1,1))

# QUESTION 2 UNHEDGED EQUITY VALUATION

# ==============================================================================
# STEP 1 - EXTRACT SIMULATION MATRICES FOR Q2/Q3
# ==============================================================================
P_e <- sim_prices[2:(n_months+1), 1, ]   # Electricity  [60 x 10000]
P_g <- sim_prices[2:(n_months+1), 2, ]   # Natural Gas  [60 x 10000]
P_c <- sim_prices[2:(n_months+1), 3, ]   # Carbon       [60 x 10000]
U   <- wind_utilization[2:(n_months+1), ] # Wind util    [60 x 10000]


# ==============================================================================
# STEP 2 - PROJECT PARAMETERS
# ==============================================================================

hours_per_month <- 30 * 24          # 720 hours
wind_cap_MW     <- 125              # MW
dc_MW           <- 50               # MW
dc_MWh          <- dc_MW * hours_per_month  # 36,000 MWh/month

coupon          <- 0.6e6            # EUR/month
principal       <- 60e6             # EUR (bullet at month 60)
equity0         <- 40e6             # EUR (upfront equity investment)

rf_annual       <- 0.02
rE              <- 0.12

# Discrete monthly compounding per assignment instructions
rf_m            <- (1 + rf_annual)^(1/12) - 1   # 0.001652


# ==============================================================================
# STEP 3 - PRODUCTION LOGIC
# ==============================================================================

wind_MWh   <- wind_cap_MW * U * hours_per_month   # actual wind produced
gas_MWh    <- pmax(0, dc_MWh - wind_MWh)          # gas fills deficit when wind < 50MW
excess_MWh <- pmax(0, wind_MWh - dc_MWh)          # excess wind sold to grid


# ==============================================================================
# STEP 4 - REVENUE
# ==============================================================================

# Data Center always receives exactly 50MW (gas fills any wind shortfall),
# so DC revenue = 36,000 MWh * PPA price every month.
# PPA price is a FIXED strike set once at t=0 as mean(sim_elec)*0.80.
# This is correct: a PPA is a forward contract, not a floating price.
# Excess wind sold to grid at full spot price (no discount).

p_ppa    <- mean(P_e) * 0.80        # fixed PPA strike price (EUR/MWh)

cat("\nPPA strike price (fixed for all 60 months): EUR", round(p_ppa, 4), "/ MWh\n")

rev_dc   <- dc_MWh * p_ppa          # DC revenue: constant volume * fixed strike
rev_grid <- excess_MWh * P_e        # grid revenue: excess wind at spot
revenue  <- rev_dc + rev_grid


# ==============================================================================
# STEP 5 - OPERATING COSTS
# ==============================================================================

cost_wind        <- wind_MWh * 20                           # EUR 20/MWh on all wind
gas_cost_per_MWh <- 45 + P_g + 0.4 * P_c                   # EUR: opex + fuel + carbon
cost_gas         <- gas_MWh * gas_cost_per_MWh

net_op_cf <- revenue - cost_wind - cost_gas                 # [60 x 10000]


# ==============================================================================
# STEP 6 - CASH ACCOUNT AND DISTRESS MACHENISM
# ==============================================================================

# Each month: balance earns rf_m, add net operating CF, subtract coupon.
# If balance < 0: equity injects the deficit (resets balance to 0).

A         <- matrix(0, nrow=n_months, ncol=n_sim)
injection <- matrix(0, nrow=n_months, ncol=n_sim)

for(i in 1:n_sim)
{
  a <- 0   # cash account starts at zero
  
  for(t in 1:n_months)
  {
    a <- a * (1 + rf_m) + net_op_cf[t, i] - coupon
    
    if(a < 0)
    {
      injection[t, i] <- -a    # record injection amount
      a <- 0                   # reset balance to zero
    }
    
    A[t, i] <- a               # store end-of-month balance
  }
}


# ==============================================================================
# STEP 7 - FINAL DIVIDEND AND NPV
# ==============================================================================

# Dividend = max(0, A_60 - principal)
# NPV = -equity0 - PV(1.10 * injections) + PV(dividend)
# The 1.10 multiplier is correct: equity pays EUR 1.10 per EUR 1.00 of deficit.

A60          <- A[n_months, ]
dividend60   <- pmax(0, A60 - principal)

# Discount vector: month t discounted at (1+rE)^(t/12) = correct discrete
disc         <- (1 + rE)^((1:n_months) / 12)   # [60]

pv_inj       <- colSums(1.10 * injection /
                          matrix(disc, nrow=n_months, ncol=n_sim))

pv_div       <- dividend60 / (1 + rE)^(n_months / 12)

equity_npv   <- -equity0 - pv_inj + pv_div


# ==============================================================================
# QUESTION 2 RESULTS
# ==============================================================================

expected_npv <- mean(equity_npv)
ci95         <- quantile(equity_npv, c(0.025, 0.975))

cat("\n==========================================\n")
cat("Q2 RESULTS — UNHEDGED EQUITY VALUATION\n")
cat("==========================================\n")
cat(sprintf("Expected Equity NPV:      EUR %s\n",
            format(round(expected_npv, 0), big.mark=",")))
cat(sprintf("95%% Confidence Interval: EUR %s  to  EUR %s\n",
            format(round(ci95[1], 0), big.mark=","),
            format(round(ci95[2], 0), big.mark=",")))

# Q2c — Probability of distress
prob_distress <- mean(injection > 0)
cat(sprintf("Prob. of Distress (per month): %.4f%%\n", prob_distress * 100))

# Q2d — Cumulative cost of distress (the 10% penalty only)
distress_cost     <- 0.10 * injection              # penalty surcharge only
cum_distress_cost <- colSums(distress_cost)        # one value per simulation

cat(sprintf("Mean Cumulative Distress Cost (penalty): EUR %s\n",
            format(round(mean(cum_distress_cost), 0), big.mark=",")))
cat("==========================================\n")

# Q2d — Distribution plot
has_distress_nominal <- cum_distress_cost > 0

hist(cum_distress_cost[has_distress_nominal],
     breaks = 60,
     main   = "Cumulative Distress Cost\n(Conditional on Distress Occurring)",
     xlab   = "Total Distress Penalty Paid (EUR)",
     ylab   = "Frequency",
     col    = "steelblue", border = "white")
abline(v = mean(cum_distress_cost[has_distress_nominal]), col="red", lwd=2)
legend("topright", legend="Conditional Mean", col="red", lwd=2, bty="n")

#QUESTION 3 KNOCK-IN SPARK SPREAD CALL OPTION (FAIR VALUE)


# ==============================================================================
# STEP 1 - OPTION PAYOFF
# ==============================================================================

# The option for month t is active IF AND ONLY IF wind utilization < 40%.
# Underlying: spark spread = Gas_t - Electricity_t
# Payoff (if active): max(0, spread) * volume
# Volume: 50MW * 720 hours = 36,000 MWh

volume_hedged <- 50 * hours_per_month          # 36,000 MWh

spark_spread  <- P_g - P_e
is_active     <- (U < 0.40) & (spark_spread > 0)

payoff        <- spark_spread * volume_hedged
payoff[!is_active] <- 0


# ==============================================================================
# STEP 2 - FAIR VALUE (PV OF EXPECTED PAYOFFS)
# ==============================================================================

disc_rf      <- (1 + rf_m)^(1:n_months)        # [60] risk-free discount factors

E_payoff_t   <- rowMeans(payoff)               # expected payoff per month
PV_t         <- E_payoff_t / disc_rf           # PV of each month's expected payoff

fair_value_Q3 <- sum(PV_t)

cat("\n==========================================\n")
cat("Q3 RESULTS — KNOCK-IN SPARK SPREAD OPTION\n")
cat("==========================================\n")
cat(sprintf("Fair Value of Derivative Strip: EUR %s\n",
            format(round(fair_value_Q3, 0), big.mark=",")))
cat("==========================================\n")


# QUESTION 4 — DOES HEDGING CREATE VALUE?

# ==============================================================================
#   a. Re-run Q2 but SPV now owns the derivative and receives monthly payoff.
#   b. Deduct the upfront cost (fair_value_Q3) from equity investment at t=0.
#   c. Analysis: new NPV, distress reduction, fair pricing, partial hedge.
#
# NOTE: Runs directly after Q1/Q2/Q3 — all objects already in memory.
#       No RDS loading or re-declaration of constants needed.
# ==============================================================================


# ==============================================================================
# STEP 1 — HEDGED NET OPERATING CASH FLOW
#
# SPV owns the derivative strip and receives payoff_Q3 each month.
# Payoff is zero or positive, arriving before the distress check.
# ==============================================================================

# Each month: normal operating CF + option payoff (>= 0)
net_op_cf_hedged <- net_op_cf + payoff     # [60 x 10000]


# ==============================================================================
# STEP 2 — HEDGED CASH ACCOUNT AND DISTRESS LOOP
# ==============================================================================

A_h         <- matrix(0, nrow=n_months, ncol=n_sim)
injection_h <- matrix(0, nrow=n_months, ncol=n_sim)

for(i in 1:n_sim)
{
  a <- 0   # cash account starts at zero
  
  for(t in 1:n_months)
  {
    a <- a * (1 + rf_m) + net_op_cf_hedged[t, i] - coupon
    
    if(a < 0)
    {
      injection_h[t, i] <- -a
      a <- 0
    }
    
    A_h[t, i] <- a
  }
}


# ==============================================================================
# STEP 3 — FINAL DIVIDEND AND PV COMPONENTS
# ==============================================================================

A60_h        <- A_h[n_months, ]
dividend60_h <- pmax(0, A60_h - principal)

pv_div_h <- dividend60_h / (1 + rE)^(n_months / 12)
pv_inj_h <- colSums(1.10 * injection_h /
                      matrix(disc, nrow=n_months, ncol=n_sim))


# ==============================================================================
# STEP 4a — NPV IF HEDGE IS FREE (GROSS BENEFIT)
#
# Isolates how much operational value the hedge creates on its own,
# before paying for it.  Answers: "how much does the hedge improve
# the project if it costs nothing?"
# ==============================================================================

equity_npv_h_free   <- -equity0 - pv_inj_h + pv_div_h
expected_npv_h_free <- mean(equity_npv_h_free)

cat("\n==========================================\n")
cat("Q4(a) — HEDGE GROSS BENEFIT (Free Hedge)\n")
cat("==========================================\n")
cat(sprintf("Expected NPV if hedge is free: EUR %s\n",
            format(round(expected_npv_h_free, 0), big.mark=",")))
cat(sprintf("Expected NPV unhedged (Q2):    EUR %s\n",
            format(round(mean(equity_npv), 0), big.mark=",")))
cat(sprintf("Gross benefit of hedge:        EUR %s\n",
            format(round(expected_npv_h_free - mean(equity_npv), 0), big.mark=",")))
cat("==========================================\n")


# ==============================================================================
# STEP 4b — NPV AFTER PAYING UPFRONT COST (NET BENEFIT)
#
# The fair value of the derivative strip is paid at t=0,
# increasing the equity outlay from EUR 40M to EUR 40M + fair_value_Q3.
#
# NPV_hedged = -(equity0 + fair_value_Q3) - PV(1.10 * injections) + PV(dividend)
# ==============================================================================

equity_invested_with_hedge <- equity0 + fair_value_Q3   # total equity outlay at t=0

equity_npv_h   <- -equity_invested_with_hedge - pv_inj_h + pv_div_h
expected_npv_h <- mean(equity_npv_h)
ci95_h         <- quantile(equity_npv_h,  c(0.025, 0.975))
ci95_unhedged  <- quantile(equity_npv,    c(0.025, 0.975))

# Distress statistics
prob_distress_h     <- mean(injection_h > 0)
cum_distress_cost_h <- colSums(0.10 * injection_h)
cum_distress_cost_u <- colSums(0.10 * injection)

cat("\n==========================================\n")
cat("Q4(b/c) — HEDGED vs UNHEDGED COMPARISON\n")
cat("==========================================\n")
cat(sprintf("Upfront Derivative Cost:        EUR %s\n",
            format(round(fair_value_Q3, 0), big.mark=",")))
cat(sprintf("Total Equity Outlay at t=0:     EUR %s\n",
            format(round(equity_invested_with_hedge, 0), big.mark=",")))
cat(sprintf("\nExpected NPV (Hedged):          EUR %s\n",
            format(round(expected_npv_h, 0), big.mark=",")))
cat(sprintf("Expected NPV (Unhedged):        EUR %s\n",
            format(round(mean(equity_npv), 0), big.mark=",")))
cat(sprintf("Net NPV change from hedging:    EUR %s\n",
            format(round(expected_npv_h - mean(equity_npv), 0), big.mark=",")))
cat(sprintf("\n95%% CI (Hedged):   EUR %s  to  EUR %s\n",
            format(round(ci95_h[1], 0), big.mark=","),
            format(round(ci95_h[2], 0), big.mark=",")))
cat(sprintf("95%% CI (Unhedged): EUR %s  to  EUR %s\n",
            format(round(ci95_unhedged[1], 0), big.mark=","),
            format(round(ci95_unhedged[2], 0), big.mark=",")))
cat(sprintf("\nProb. Distress per month (Hedged):   %.4f%%\n",
            prob_distress_h * 100))
cat(sprintf("Prob. Distress per month (Unhedged): %.4f%%\n",
            mean(injection > 0) * 100))
cat(sprintf("\nMean Cum. Distress Cost (Hedged):   EUR %s\n",
            format(round(mean(cum_distress_cost_h), 0), big.mark=",")))
cat(sprintf("Mean Cum. Distress Cost (Unhedged): EUR %s\n",
            format(round(mean(cum_distress_cost_u), 0), big.mark=",")))
cat("==========================================\n")


# ==============================================================================
# STEP 5 — COMPARISON PLOT: HEDGED vs UNHEDGED NPV DISTRIBUTIONS
# ==============================================================================

xlim_range <- range(c(equity_npv_h, equity_npv))

par(mfrow=c(1,2), mar=c(5,4,4,2))

hist(equity_npv,
     breaks = 80,
     main   = "Unhedged Equity NPV",
     xlab   = "NPV (EUR)",
     col    = "steelblue", border = "white",
     xlim   = xlim_range)
abline(v = mean(equity_npv), col="red", lwd=2)
legend("topright", legend="Mean", col="red", lwd=2, bty="n")

hist(equity_npv_h,
     breaks = 80,
     main   = "Hedged Equity NPV",
     xlab   = "NPV (EUR)",
     col    = "coral", border = "white",
     xlim   = xlim_range)
abline(v = mean(equity_npv_h), col="red", lwd=2)
legend("topright", legend="Mean", col="red", lwd=2, bty="n")

par(mfrow=c(1,1))


# ==============================================================================
# STEP 6 — PARTIAL HEDGE ANALYSIS
#
# Q4c: "Would it be better to not fully hedge the full capacity (50MW)?"
# Test hedge ratios 0% to 100% in steps of 10%.
# Both payoff and upfront cost scale linearly with the hedge ratio.
# ==============================================================================

hedge_ratios  <- seq(0, 1, by=0.10)
npv_by_ratio  <- numeric(length(hedge_ratios))
dist_by_ratio <- numeric(length(hedge_ratios))

for(h in seq_along(hedge_ratios))
{
  hr <- hedge_ratios[h]
  
  net_op_cf_partial <- net_op_cf + hr * payoff
  
  A_p         <- matrix(0, nrow=n_months, ncol=n_sim)
  injection_p <- matrix(0, nrow=n_months, ncol=n_sim)
  
  for(i in 1:n_sim)
  {
    a <- 0
    for(t in 1:n_months)
    {
      a <- a * (1 + rf_m) + net_op_cf_partial[t, i] - coupon
      if(a < 0) { injection_p[t, i] <- -a; a <- 0 }
      A_p[t, i] <- a
    }
  }
  
  A60_p    <- A_p[n_months, ]
  div60_p  <- pmax(0, A60_p - principal)
  pv_inj_p <- colSums(1.10 * injection_p /
                        matrix(disc, nrow=n_months, ncol=n_sim))
  pv_div_p <- div60_p / (1 + rE)^(n_months / 12)
  
  npv_p <- -(equity0 + hr * fair_value_Q3) - pv_inj_p + pv_div_p
  
  npv_by_ratio[h]  <- mean(npv_p)
  dist_by_ratio[h] <- mean(injection_p > 0)
}

cat("\n==========================================\n")
cat("PARTIAL HEDGE ANALYSIS\n")
cat("==========================================\n")
cat(sprintf("%-14s %-22s %-20s\n",
            "Hedge Ratio", "Expected NPV (EUR)", "P(Distress/month)"))
for(h in seq_along(hedge_ratios))
{
  cat(sprintf("%-14.0f%% %-22s %.4f%%\n",
              hedge_ratios[h] * 100,
              format(round(npv_by_ratio[h], 0), big.mark=","),
              dist_by_ratio[h] * 100))
}
cat("==========================================\n")

par(mfrow=c(1,2), mar=c(5,5,4,2))

plot(hedge_ratios * 100, npv_by_ratio / 1e6,
     type="b", pch=19, col="steelblue", lwd=2,
     main="Expected NPV vs Hedge Ratio",
     xlab="Hedge Ratio (%)", ylab="Expected Equity NPV (EUR Million)")
abline(h = mean(equity_npv) / 1e6, col="grey50", lty=2)
legend("bottomleft",
       legend=c("Hedged NPV", "Unhedged NPV"),
       col=c("steelblue","grey50"), lty=c(1,2), lwd=2, bty="n")

plot(hedge_ratios * 100, dist_by_ratio * 100,
     type="b", pch=19, col="coral", lwd=2,
     main="Distress Probability vs Hedge Ratio",
     xlab="Hedge Ratio (%)", ylab="P(Distress per month) (%)")

par(mfrow=c(1,1))

cat("\nQ4 complete.\n")


# QUESTION 5 — STRESS TESTING

# ==============================================================================
#   a. Override the Gas-Electricity correlation to 0.0 in the covariance matrix.
#   b. Re-simulate market variables and recalculate Q2 (Unhedged Equity NPV)
#      and Q3 (Derivative Fair Value).
#   c. Compare results to the baseline and explain the economic intuition.
#
# NOTE: Runs directly after Q1–Q4 — all objects already in memory.
#       Baseline results (equity_npv, fair_value_Q3) are preserved for comparison.
# ==============================================================================


# ==============================================================================
# STEP 1 — BUILD STRESSED COVARIANCE MATRIX
#
# The original Sigma has columns/rows ordered:
#   1 = Electricity, 2 = WindUtilization, 3 = NaturalGas, 4 = Carbon
#
# We set the Gas-Electricity covariance to zero while keeping all
# individual variances and all other correlations unchanged.
#
# Relationship between covariance and correlation:
#   Cov(X,Y) = Cor(X,Y) * SD(X) * SD(Y)
# Setting Cor(Gas, Elec) = 0 means Cov(Gas, Elec) = 0.
# We zero out both [1,3] and [3,1] (symmetric matrix).
# ==============================================================================

Sigma_stress        <- Sigma          # copy baseline covariance matrix
Sigma_stress[1, 3]  <- 0             # Cov(Electricity, NaturalGas) = 0
Sigma_stress[3, 1]  <- 0             # symmetric

# Verify the stressed correlation
SD        <- sqrt(diag(Sigma_stress))
cor_stress <- Sigma_stress / outer(SD, SD)

cat("\n==========================================\n")
cat("Q5 — STRESSED CORRELATION MATRIX\n")
cat("==========================================\n")
cat("Baseline Gas-Electricity correlation:  ",
    round(cor_matrix[1, 3], 4), "\n")
cat("Stressed Gas-Electricity correlation:  ",
    round(cor_stress[1, 3],  4), "\n")
cat("\nFull stressed correlation matrix:\n")
print(round(cor_stress, 4))
cat("==========================================\n")

# Verify Sigma_stress is still positive semi-definite
eigenvalues <- eigen(Sigma_stress)$values
cat("\nEigenvalues of Sigma_stress (all must be >= 0):\n")
print(round(eigenvalues, 6))
if(all(eigenvalues >= -1e-10))
  cat("Sigma_stress is valid (positive semi-definite).\n")


# ==============================================================================
# STEP 2 — RE-SIMULATE UNDER STRESSED COVARIANCE
#
# Everything is identical to Q1 except Sigma is replaced by Sigma_stress.
# Same seed for direct comparability with baseline.
# ==============================================================================

set.seed(42)

sim_returns_s <- array(0, dim=c(n_months, n_assets, n_sim))

for(i in 1:n_sim)
{
  sim_returns_s[,,i] <- rmvnorm(
    n     = n_months,
    mean  = mu,
    sigma = Sigma_stress
  )
}

# Price paths — identical structure to Q1
sim_prices_s <- array(0, dim=c(n_months+1, n_prices, n_sim))

for(i in 1:n_sim)
{
  sim_prices_s[1,,i] <- initial_prices
  
  for(t in 2:(n_months+1))
  {
    sim_prices_s[t,,i] <- sim_prices_s[t-1,,i] *
      exp(sim_returns_s[t-1, price_cols, i])
  }
}

# Wind logit path — identical structure to Q1
sim_wind_logit_s <- matrix(0, nrow=n_months+1, ncol=n_sim)

for(i in 1:n_sim)
{
  sim_wind_logit_s[1, i] <- wind_logit0
  
  for(t in 2:(n_months+1))
  {
    sim_wind_logit_s[t, i] <- sim_wind_logit_s[t-1, i] +
      sim_returns_s[t-1, 2, i]
  }
}

wind_utilization_s <- exp(sim_wind_logit_s) / (1 + exp(sim_wind_logit_s))

# Extract months 1..60
P_e_s <- sim_prices_s[2:(n_months+1), 1, ]
P_g_s <- sim_prices_s[2:(n_months+1), 2, ]
P_c_s <- sim_prices_s[2:(n_months+1), 3, ]
U_s   <- wind_utilization_s[2:(n_months+1), ]


# ==============================================================================
# STEP 3 — Q3 UNDER STRESS: DERIVATIVE FAIR VALUE
#
# Recalculate the knock-in spark spread option fair value
# using the stressed price paths.
# ==============================================================================

spark_spread_s          <- P_g_s - P_e_s
is_active_s             <- (U_s < 0.40) & (spark_spread_s > 0)
payoff_s                <- spark_spread_s * volume_hedged
payoff_s[!is_active_s]  <- 0

E_payoff_t_s    <- rowMeans(payoff_s)
PV_t_s          <- E_payoff_t_s / disc_rf
fair_value_Q3_s <- sum(PV_t_s)


# ==============================================================================
# STEP 4 — Q2 UNDER STRESS: UNHEDGED EQUITY NPV
#
# Recalculate the full Q2 cash flow waterfall using stressed price paths.
# PPA strike is recomputed from the stressed electricity paths.
# ==============================================================================

# Production logic
wind_MWh_s   <- wind_cap_MW * U_s * hours_per_month
gas_MWh_s    <- pmax(0, dc_MWh - wind_MWh_s)
excess_MWh_s <- pmax(0, wind_MWh_s - dc_MWh)

# PPA strike recomputed from stressed simulation
p_ppa_s  <- mean(P_e_s) * 0.80

rev_dc_s   <- dc_MWh * p_ppa_s
rev_grid_s <- excess_MWh_s * P_e_s
revenue_s  <- rev_dc_s + rev_grid_s

cost_wind_s        <- wind_MWh_s * 20
gas_cost_per_MWh_s <- 45 + P_g_s + 0.4 * P_c_s
cost_gas_s         <- gas_MWh_s * gas_cost_per_MWh_s

net_op_cf_s <- revenue_s - cost_wind_s - cost_gas_s

# Cash account and distress loop
A_s         <- matrix(0, nrow=n_months, ncol=n_sim)
injection_s <- matrix(0, nrow=n_months, ncol=n_sim)

for(i in 1:n_sim)
{
  a <- 0
  for(t in 1:n_months)
  {
    a <- a * (1 + rf_m) + net_op_cf_s[t, i] - coupon
    if(a < 0)
    {
      injection_s[t, i] <- -a
      a <- 0
    }
    A_s[t, i] <- a
  }
}

A60_s        <- A_s[n_months, ]
dividend60_s <- pmax(0, A60_s - principal)

pv_div_s <- dividend60_s / (1 + rE)^(n_months / 12)
pv_inj_s <- colSums(1.10 * injection_s /
                      matrix(disc, nrow=n_months, ncol=n_sim))

equity_npv_s <- -equity0 - pv_inj_s + pv_div_s


# ==============================================================================
# STEP 5 — RESULTS: BASELINE vs STRESSED
# ==============================================================================

cat("\n==========================================\n")
cat("Q5 RESULTS — BASELINE vs STRESSED\n")
cat("==========================================\n")
cat(sprintf("%-35s %-20s %-20s\n",
            "", "Baseline", "Stressed (rho=0)"))
cat(sprintf("%-35s %-20s %-20s\n",
            "Gas-Elec Correlation",
            round(cor_matrix[1,3], 4),
            round(cor_stress[1,3], 4)))
cat(sprintf("%-35s %-20s %-20s\n",
            "Derivative Fair Value (Q3)",
            format(round(fair_value_Q3,   0), big.mark=","),
            format(round(fair_value_Q3_s, 0), big.mark=",")))
cat(sprintf("%-35s %-20s %-20s\n",
            "Expected Equity NPV (Q2)",
            format(round(mean(equity_npv),   0), big.mark=","),
            format(round(mean(equity_npv_s), 0), big.mark=",")))
cat(sprintf("%-35s %-20s %-20s\n",
            "Prob. Distress per month",
            paste0(round(mean(injection   > 0) * 100, 4), "%"),
            paste0(round(mean(injection_s > 0) * 100, 4), "%")))
cat(sprintf("%-35s %-20s %-20s\n",
            "Mean Cum. Distress Cost",
            format(round(mean(colSums(0.10 * injection)),   0), big.mark=","),
            format(round(mean(colSums(0.10 * injection_s)), 0), big.mark=",")))
cat("==========================================\n")


# ==============================================================================
# STEP 6 — COMPARISON PLOT: BASELINE vs STRESSED NPV DISTRIBUTIONS
# ==============================================================================

xlim_range_s <- range(c(equity_npv, equity_npv_s))

par(mfrow=c(1,2), mar=c(5,4,4,2))

hist(equity_npv,
     breaks = 80,
     main   = "Unhedged NPV — Baseline",
     xlab   = "NPV (EUR)",
     col    = "steelblue", border = "white",
     xlim   = xlim_range_s)
abline(v = mean(equity_npv), col="red", lwd=2)
legend("topright", legend="Mean", col="red", lwd=2, bty="n")

hist(equity_npv_s,
     breaks = 80,
     main   = "Unhedged NPV — Stressed (rho=0)",
     xlab   = "NPV (EUR)",
     col    = "coral", border = "white",
     xlim   = xlim_range_s)
abline(v = mean(equity_npv_s), col="red", lwd=2)
legend("topright", legend="Mean", col="red", lwd=2, bty="n")

par(mfrow=c(1,1))

cat("\nQ5 complete.\n")
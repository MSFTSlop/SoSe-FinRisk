# ==============================================================================
# TASK 2: RISK-ADJUSTED VALUATION & REVENUE DYNAMICS
# ==============================================================================

# ------------------------------------------------------------------------------
# 2a. MONTHLY PRODUCTION & REVENUE BREAKDOWN
# ------------------------------------------------------------------------------
# Step 1: Physical Generation (125MW capacity)
# Formula: 125 * Utilization % * 720 hours
prod_total_mwh <- project_params$cap_wind * sim_util * project_params$hours_mo

# Step 2: Revenue from Electricity Sales to DATACENTER (PPA)
# SPV sells up to 50MW at a fixed price (p_ppa).
p_ppa <- 45 # Benchmark PPA Price
prod_ppa_mwh <- pmin(prod_total_mwh, project_params$demand_ppa * project_params$hours_mo)
rev_datacenter <- prod_ppa_mwh * p_ppa

# Step 3: Revenue from Electricity Sales to GRID (Merchant)
# Excess energy sold at the simulated spot price (sim_elec)
prod_merch_mwh <- pmax(0, prod_total_mwh - (project_params$demand_ppa * project_params$hours_mo))
rev_grid <- prod_merch_mwh * sim_elec

# Step 4: Total Revenue (The Top Line)
rev_total <- rev_datacenter + rev_grid


# ------------------------------------------------------------------------------
# 2b. EXPECTED NET PRESENT VALUE (NPV)
# ------------------------------------------------------------------------------
# We calculate the NPV for each of the 10,000 paths to find the average value.
# Discount Rate: 10% annual (0.83% monthly)
r_monthly <- 0.10 / 12
discount_factors <- 1 / (1 + r_monthly)^(1:n_months)

# Matrix calculation: (Monthly Revenue * Discount Factor) summed for each path
path_npv <- colSums(rev_total[2:61, ] * discount_factors)
expected_npv <- mean(path_npv)


# ------------------------------------------------------------------------------
# 2c. PROBABILITY OF DISTRESS (MONTHLY)
# ------------------------------------------------------------------------------
# 'Distress' is defined as any month where Revenue < Monthly Debt Service.
# Monthly Coupon: €0.6M (from project_params)
debt_service <- project_params$monthly_coupon

# We check every month (rows) across every path (columns)
distress_matrix <- rev_total[2:61, ] < debt_service

# Probability is the average number of 'TRUE' results in the entire matrix
prob_distress_monthly <- mean(distress_matrix)


# ------------------------------------------------------------------------------
# 2d. CUMULATIVE PROBABILITY OF DISTRESS
# ------------------------------------------------------------------------------
# This measures the probability that the project fails AT LEAST ONCE 
# by a certain month.
# 'cumany' checks if a path has hit 'TRUE' (Distress) at any point in time.
ever_failed <- apply(distress_matrix, 2, cumsum) > 0
cum_prob_distress <- rowMeans(ever_failed) # Result is a vector for months 1 to 60


# ==============================================================================
# FINAL TASK 2 OUTPUT SUMMARY
# ==============================================================================
cat("\n--- TASK 2: SUMMARY RESULTS ---\n")
cat("2a. Avg Monthly Revenue:      €", round(mean(rev_total), 2), "\n")
cat("2b. Expected NPV (Project):   €", round(expected_npv, 2), "\n")
cat("2c. Monthly Prob. of Distress:", round(prob_distress_monthly * 100, 2), "%\n")
cat("2d. Final Cumulative Risk:    ", round(tail(cum_prob_distress, 1) * 100, 2), "%\n")
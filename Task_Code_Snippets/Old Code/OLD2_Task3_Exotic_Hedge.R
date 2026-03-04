# ==============================================================================
# TASK 3: EXOTIC HEDGE VALUATION & CORRELATION AUDIT
# ==============================================================================

# 0. SAFEGUARD: Check for existence of required universes
# ------------------------------------------------------------------------------
current_mode_label <- if (exists("stress_test_mode") && stress_test_mode == TRUE) "STRESS" else "BASE"

if (current_mode_label == "STRESS" && !exists("sim_universe_stressed")) {
  stop("FATAL ERROR: 'sim_universe_stressed' not found. Run Task 1.2 in Stress Mode first.")
}
if (current_mode_label == "BASE" && !exists("sim_universe_base")) {
  stop("FATAL ERROR: 'sim_universe_base' not found. Run Task 1.2 in Base Mode first.")
}

# 1. EXTRACT SECURED MATRICES (EXPLICIT ROUTING)
# ------------------------------------------------------------------------------
switch(current_mode_label,
       "BASE" = {
         sim_elec <- sim_universe_base$sim_elec
         sim_gas  <- sim_universe_base$sim_gas
         sim_util <- sim_universe_base$sim_util
       },
       "STRESS" = {
         sim_elec <- sim_universe_stressed$sim_elec
         sim_gas  <- sim_universe_stressed$sim_gas
         sim_util <- sim_universe_stressed$sim_util
       }
)

# 2. THE PAYOFF LOGIC (Consistent for both scenarios)
# ------------------------------------------------------------------------------
spread_matrix  <- sim_gas[2:61, ] - sim_elec[2:61, ] 
trigger_matrix <- sim_util[2:61, ] < 0.40           
mwh_volume     <- 50 * 720                           
payoff_matrix  <- trigger_matrix * pmax(0, spread_matrix) * mwh_volume

# 3. DISCOUNTING TO FAIR VALUE
# ------------------------------------------------------------------------------
r_monthly      <- 0.02 / 12
discount_vec   <- 1 / (1 + r_monthly)^(1:60)
path_hedge_pvs <- colSums(payoff_matrix * discount_vec)
fv_hedge       <- mean(path_hedge_pvs)

# 4. ARCHIVING & THE AUDIT REPORT
# ------------------------------------------------------------------------------
current_hedge <- list(
  fair_value    = fv_hedge,
  payoff_matrix = payoff_matrix,
  path_pvs      = path_hedge_pvs
)

cat("\n================================================================\n")
cat("TASK 3: HEDGE VALUATION & STATISTICAL AUDIT\n")
cat("================================================================\n")

switch(current_mode_label,
       "BASE" = {
         hedge_base <- current_hedge
         corr_base  <- cor(as.vector(sim_elec[2:61,]), as.vector(sim_gas[2:61,]))
         
         cat(sprintf("Fair Value (Base Case):      €%s\n", format(round(fv_hedge, 2), big.mark=",")))
         cat(sprintf("- Base Corr (Elec/Gas):      %s\n", round(corr_base, 4)))
         cat("\n>>> 'hedge_base' successfully secured for Task 2.\n")
       },
       
       "STRESS" = {
         hedge_stressed <- current_hedge
         corr_stressed  <- cor(as.vector(sim_elec[2:61,]), as.vector(sim_gas[2:61,]))
         
         cat(sprintf("Fair Value (Stress Test):    €%s\n", format(round(fv_hedge, 2), big.mark=",")))
         cat(sprintf("- Stressed Corr (Elec/Gas):  %s (Approaching Zero)\n", round(corr_stressed, 4)))
         
         # Generate the comparative insight ONLY if the Base Case has already been run
         if (exists("hedge_base") && exists("sim_universe_base")) {
           corr_base  <- cor(as.vector(sim_universe_base$sim_elec[2:61,]), 
                             as.vector(sim_universe_base$sim_gas[2:61,]))
           impact_val <- hedge_stressed$fair_value - hedge_base$fair_value
           
           cat("----------------------------------------------------------------\n")
           cat("CORRELATION VERIFICATION & IMPACT:\n")
           cat(sprintf("- Base Corr (Elec/Gas):      %s\n", round(corr_base, 4)))
           cat(sprintf("Correlation Impact (Value):  €%s\n", format(round(impact_val, 2), big.mark=",")))
           
           cat("\nTHEORETICAL INSIGHT:\n")
           if(hedge_stressed$fair_value > hedge_base$fair_value) {
             cat("The hedge is CHEAPER when correlated. High correlation means Gas and\n")
             cat("Elec move together, naturally 'shrinking' the spread payoffs.\n")
             cat("Decoupling forces the SPV to rely heavily on this derivative.\n")
           } else {
             cat("The hedge is MORE EXPENSIVE when correlated. This suggests the \n")
             cat("spread widens under historical market conditions.\n")
           }
         } else {
           cat("\n(Run Base Case first to see comparative Correlation Impact)\n")
         }
         cat("\n>>> 'hedge_stressed' successfully secured for Task 2.\n")
       }
)
cat("================================================================\n")

# ==============================================================================
# 5. WORKSPACE SANITIZATION (THE COURTESY CLEANUP)
# ==============================================================================
# Remove temporary matrices and variables to prevent memory leaks and ghost data.

# Determine which correlation variables exist so we don't throw an error when removing them
vars_to_remove <- c("sim_elec", "sim_gas", "sim_util", "spread_matrix", 
                    "trigger_matrix", "payoff_matrix", "path_hedge_pvs", 
                    "current_hedge", "r_monthly", "discount_vec", "fv_hedge", 
                    "mwh_volume", "current_mode_label")

if (exists("corr_base")) vars_to_remove <- c(vars_to_remove, "corr_base")
if (exists("corr_stressed")) vars_to_remove <- c(vars_to_remove, "corr_stressed")
if (exists("impact_val")) vars_to_remove <- c(vars_to_remove, "impact_val")

rm(list = vars_to_remove)

cat(">>> Workspace sanitized. Temporary Task 3 variables cleared.\n")

# ==============================================================================
# DATA LINEAGE & VARIABLE ANCESTRY (TASK 1 -> TASK 2 -> TASK 3)
# ==============================================================================
# To ensure mathematical consistency across the project, all Task 3 variables 
# are derived from a single "Source of Truth" established in Tasks 1 and 2:
#
# 1. THE ANCHORS (T=0: January 2026):
#    - p_elec_0 (135.02), p_gas_0 (75.705), p_carb_0 (79.8), u_wind_0 (0.3789).
#    - These are the last known real-world data points from your Excel 'Last_Price'.
#
# 2. THE SIMULATION ENGINES (Task 1.2):
#    - sim_elec & sim_gas: Generated via Geometric Brownian Motion (GBM). 
#      They evolve from the T=0 Anchors using the Drift (mu) and Volatility (sigma) 
#      calculated from the historical 'Returns' dataframe.
#    - sim_util: Generated via an Additive Random Walk in Logit-Space (l_wind_0).
#      The results were transformed back to [0,1] using the Sigmoid function:
#      inv_logit = exp(L) / (1 + exp(L)).
#
# 3. THE REVENUE STACK (Task 2):
#    - Used sim_util and sim_elec to calculate 'rev_total'.
#    - Established the 50MW PPA / 125MW Capacity relationship.
#
# 4. THE HEDGE UNDERLYING (Task 3):
#    - sim_gas & sim_elec: Used here to calculate the "Spark Spread" (Gas - Elec).
#    - sim_util: Used here to trigger the "Knock-In" (Shortfall if Wind < 40%).
#
# VERDICT: By using these forecast matrices instead of historical 'shuffling',
# Task 3 evaluates the hedge against 10,000 "Possible Futures" rather than 
# simply re-playing the past.
# ==============================================================================
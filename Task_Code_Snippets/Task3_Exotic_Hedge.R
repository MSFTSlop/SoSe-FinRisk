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
message("Extracting Case Specific Variables")

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

message("Calculating the mathematical logic")

# 2. THE PAYOFF LOGIC (Consistent for both scenarios)
# ------------------------------------------------------------------------------
# STRICT COMPLIANCE TO PROMPT: 
# The assignment explicitly defines the underlying as: Gas Price - Electricity Price.
# NOTE: In a real-world scenario, a Clean Spark Spread would include the cost of 
# Carbon (EUA) allowances. By excluding Carbon here as instructed, the SPV is 
# left exposed to 'Basis Risk' if Carbon prices spike while Gas remains flat.
spread_matrix  <- sim_gas[2:61, ] - sim_elec[2:61, ] 

# Option is only active (Knock-in) when wind utilization is below 40%
trigger_matrix <- sim_util[2:61, ] < 0.40            
mwh_volume     <- 50 * 720                           
payoff_matrix  <- trigger_matrix * pmax(0, spread_matrix) * mwh_volume



# 3. DISCOUNTING TO FAIR VALUE (ISSUE 9 FIX APPLIED)
# ------------------------------------------------------------------------------
r_annual       <- 0.02
# Fixed: Using discrete compounding to match Task 2 consistency
r_monthly      <- (1 + r_annual)^(1/12) - 1 
discount_vec   <- 1 / (1 + r_monthly)^(1:60)
path_hedge_pvs <- colSums(sweep(payoff_matrix, 1, discount_vec, FUN = "*"))
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
# Determine which correlation variables exist so we don't throw an error when removing them
message("Cleanup after Calculation")

vars_to_remove <- c("sim_elec", "sim_gas", "sim_util", "spread_matrix", 
                    "trigger_matrix", "payoff_matrix", "path_hedge_pvs", 
                    "current_hedge", "r_annual", "r_monthly", "discount_vec", "fv_hedge", 
                    "mwh_volume", "current_mode_label")

if(exists("base_done") && exists("hedge_done")) {
  message("Removing all heavy weight matrices.")
  if (exists("corr_base")) vars_to_remove <- c(vars_to_remove, "corr_base")
  if (exists("corr_stressed")) vars_to_remove <- c(vars_to_remove, "corr_stressed")
  if (exists("impact_val")) vars_to_remove <- c(vars_to_remove, "impact_val")
}

rm(list = vars_to_remove)

cat(">>> Workspace sanitized. Temporary Task 3 variables cleared.\n")
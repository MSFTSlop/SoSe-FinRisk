# Data Prep
  # Construct a sole windUtil Dataframe based on past data
windUtil_df <- return_factors_df
windUtil_df$Carbon <- NULL

  # Random Startpoint basend on interval of datapoints (code from gemini)

# --- 0.5. Interactive Input Loop ---
# This ensures the script stays in a loop until a valid Y or N is provided.
valid_input <- FALSE
while (!valid_input) {
  user_choice <- readline(prompt = "Run Stress Test (Zero Correlation)? [y/n]: ")
  user_choice <- tolower(user_choice) # Convert to lowercase for easier matching
  
  if (user_choice %in% c("y", "n")) {
    stress_test_mode <- (user_choice == "y")
    valid_input <- TRUE
  } else {
    cat("Invalid input. Please enter 'y' for Yes or 'n' for No.\n")
  }
}

# --- 1. Dynamic Strip Selection ---
# Formula: N - (L - 1) where N is total rows (180) and L is strip length (60)
# This guarantees a random starting point that always allows for 60 consecutive points.
random_start <- sample(1:(nrow(windUtil_df) - 59), 1)
sixty_day_sample_df  <- windUtil_df[random_start:(random_start + 59), ]

# --- 1.5. Stress Test Manipulation ---
# Mathematical Optimization: Stochastic Search
# We are seeking a permutation of Gas prices (G*) such that Corr(G*, E) -> 0
if (stress_test_mode) {
  cat("Searching for a zero-correlation permutation...\n")
  
  # Initialize the search
  best_gas <- sixty_day_sample_df$NaturalGas
  best_cor <- abs(cor(best_gas, sixty_day_sample_df$Electricity))
  
  # Trial-and-error: We simulate a 'Monte Carlo' search for the lowest correlation
  for (i in 1:10000) {
    # Generate a random permutation (shuffled sequence)
    temp_gas <- sample(sixty_day_sample_df$NaturalGas)
    temp_cor <- abs(cor(temp_gas, sixty_day_sample_df$Electricity))
    
    # If this permutation is closer to zero than our previous best, save it
    if (temp_cor < best_cor) {
      best_cor <- temp_cor
      best_gas <- temp_gas
    }
    
    # Exit condition: If we reach near-perfect zero, stop searching
    if (best_cor < 1e-5) break
  }
  sixty_day_sample_df$NaturalGas <- best_gas
}

# --- 2. Operational Constants ---
contract_target_mw <- 50   # Customer requirement
hours_month <- 30 * 24     # 720 hours (Based on 30 days 24/7 operation)
r_monthly <- 0.02 / 12     # Monthly discount rate (2% annual)

# --- 3. Derivative Payoff Logic ---

# A. The Knock-In Trigger (T_t)
# Logic: T_t = 1 if WindUtilization < 0.40, else 0
# Note: 40% of 125MW capacity = 50MW requirement.
sixty_day_sample_df$knock_in_active <- sixty_day_sample_df$WindUtilization < 0.40

# B. The Underlying: Implied Spark Spread (S_t)
# Formula: S_t = NaturalGas - Electricity
sixty_day_sample_df$spark_spread <- sixty_day_sample_df$NaturalGas - sixty_day_sample_df$Electricity

# C. The Option Payoff (P_t)
# Formula: P_t = T_t * pmax(0, S_t) * (50 * 720)
# pmax(0, S_t) represents the Call Option: max(0, Gas - Elec).
# This ensures you only receive a payoff if burning gas is a loss.
# We multiply by the full 50MW monthly volume (36,000 MWh).
sixty_day_sample_df$payoff <- ifelse(sixty_day_sample_df$knock_in_active, 
                          pmax(0, sixty_day_sample_df$spark_spread) * (contract_target_mw * hours_month), 
                          0)

# --- 4. Fair Value Calculation (PV) ---
# Formula: FV = sum( Payoff_t / (1 + r)^t )
# This discounts each of the 60 monthly payoffs back to the present value.
sixty_day_sample_df$t <- 1:60
sixty_day_sample_df$pv_payoff <- sixty_day_sample_df$payoff / (1 + r_monthly)^sixty_day_sample_df$t

fair_value <- sum(sixty_day_sample_df$pv_payoff)

# --- 5. Stress Test Baseline (Correlation) ---
# Calculates the Pearson correlation between the two underlying price drivers.
# Important for Task D: Stress testing assumes this value is pushed to 0.
current_corr <- cor(sixty_day_sample_df$NaturalGas, sixty_day_sample_df$Electricity)

# --- Display Results ---
print(paste("Random Start Point:", random_start))
print(paste("Fair Value of the 60-Month Strip:", round(fair_value, 2)))
print(paste("Current Gas-Elec Correlation in Strip:", round(current_corr, 4)))
# ==============================================================================
# 1. DATA INGESTION & PREP
# ==============================================================================
library(readxl)
library(moments)

## set your WID manually to the main.R file

last_price_df     <- read_excel("data/DataPost_2026.xlsx", sheet = "Last_Price")
returns_df        <- read_excel("data/DataPost_2026.xlsx", sheet = "Returns")
colnames(last_price_df)[1] <- "Date"
returns_df <- returns_df[, 1:5] 

source("Task_Code_Snippets/Data_Prep.R")

# ==============================================================================
# 2. INTERACTIVE USER CONFIGURATION (MOVED UP)
# ==============================================================================
# We must define the 'Scenario DNA' here so Task 1.2 knows how to simulate.

# PROMPT 1: STRESS TEST MODE (Correlation Decoupling)
valid_input_t3 <- FALSE
while (!valid_input_t3) {
  user_choice <- tolower(readline(prompt = "Run Stress Test (Zero Correlation)? [y/n]: "))
  if (user_choice %in% c("y", "n")) {
    stress_test_mode <- (user_choice == "y")
    valid_input_t3   <- TRUE
  } else {
    cat("Invalid input. Please enter 'y' for Yes or 'n' for No.\n")
  }
}

# PROMPT 2: HEDGE ACTIVATION
valid_input_t4 <- FALSE
while (!valid_input_t4) {
  user_choice <- tolower(readline(prompt = "Apply Task 3 Hedge to Task 4 Cash Flows? [y/n]: "))
  if (user_choice %in% c("y", "n")) {
    hedge_cashflow_calculator <- (user_choice == "y")
    valid_input_t4             <- TRUE
  } else {
    cat("Invalid input. Please enter 'y' for Yes or 'n' for No.\n")
  }
}

# ==============================================================================
# 3. EXECUTION CHAIN
# ==============================================================================

# Task 1: Analysis & Simulation
message("Start model Choice engine")
source("Task_Code_Snippets/Task1_1_Model_Analysis.R")

message("Choice complete. Execute Modelling")
# NOTE: Task 1.2 now checks 'stress_test_mode' to decide on Cor_Mat vs Identity_Mat
source("Task_Code_Snippets/Task1_2_Simulation.R")

# Task 3: The Hedge (Respects stress_test_mode)
message("Start Task 3. The Hedge")
source("Task_Code_Snippets/Task3_Exotic_Hedge.R")

# Task 2 & 4: Cashflow Waterfall
if (hedge_cashflow_calculator == TRUE) {
  message("starting hedge Cashflow calculation")
  source("Task_Code_Snippets/Task2_and_4_Calculation.R")
  source("Task_Code_Snippets/Task4_Analysis.R")
  message("Testing optimal hedge levels")
  source("Task_Code_Snippets/Task4_optimal_hedge.R")
} else {
  message("starting pure calculation")
  source("Task_Code_Snippets/Task2_and_4_Calculation.R")
  message("plotting npv distribution (2b) and cumulative probability of distress (2d)")
  source("Task_Code_Snippets/Task2_Plot.R")
}

# Task 5: Stress Analysis
if (stress_test_mode == TRUE) {
  message("Performing stress test analysis")
  source("Task_Code_Snippets/Task5_Stress_Analysis.R")
} else {
  message("Stress Test inactive")
}
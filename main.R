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
# UNIFIED SCENARIO SELECTOR
# ==============================================================================

## relevant for task 4 and 5 to redo calculations on task 2 and 3
cat("\nSELECT PROJECT SCENARIO:\n",
    "1: Base Case (Unhedged)\n",
    "2: Hedged Case (Task 4)\n",
    "3: Stress Test (Corr = 0)\n")

choice <- readline(prompt = "Selection [1-3]: ")

# R's version of the Java 'case' statement
switch(choice,
       "1" = {
         stress_test_mode          <- FALSE
         hedge_cashflow_calculator <- FALSE
         mode_label                <- "UNHEDGED"
       },
       "2" = {
         stress_test_mode          <- FALSE
         hedge_cashflow_calculator <- TRUE
         mode_label                <- "HEDGED"
       },
       "3" = {
         stress_test_mode          <- TRUE
         hedge_cashflow_calculator <- FALSE
         mode_label                <- "STRESSED UNHEDGED"
       },
       { # Default case (like Java's 'default')
         stop("Invalid selection. Script terminated.")
       }
)

cat(paste(">>> Initializing Scenario:", mode_label, "\n"))

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

# ==============================================================================
# Task 2 & 4: Cashflow Waterfall
# ==============================================================================
if (mode_label == "HEDGED") {
  
  message("Starting hedge Cashflow calculation")
  source("Task_Code_Snippets/Task2_and_4_Calculation.R")
  source("Task_Code_Snippets/Task4_Analysis.R")
  
  message("Testing optimal hedge levels")
  source("Task_Code_Snippets/Task4_optimal_hedge.R")
  
} else {
  # This block automatically catches both "UNHEDGED" and "STRESSED UNHEDGED"
  message("Starting pure calculation")
  source("Task_Code_Snippets/Task2_and_4_Calculation.R")
  
  message("Plotting NPV distribution (2b) and cumulative probability of distress (2d)")
  source("Task_Code_Snippets/Task2_Plot.R")
  
}

# ==============================================================================
# Task 5: Stress Analysis
# ==============================================================================
if (mode_label == "STRESSED UNHEDGED") {
  
  message("Performing stress test analysis")
  source("Task_Code_Snippets/Task5_Stress_Analysis.R")
  
} else {
  
  message("Stress Test inactive")
  
}
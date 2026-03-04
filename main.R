# ==============================================================================
# 1. DATA INGESTION & PREP
# ==============================================================================
library(readxl)
library(moments)
library(rugarch)

## set your WID manually to the main.R file

last_price_df     <- read_excel("data/DataPost_2026.xlsx", sheet = "Last_Price")
returns_df        <- read_excel("data/DataPost_2026.xlsx", sheet = "Returns")
colnames(last_price_df)[1] <- "Date"
returns_df <- returns_df[, 1:5] 

message("Initiating Data Preparation")
source("Task_Code_Snippets/Data_Prep.R")

message("Data Preparation done; Starting model Choice engine")
source("Task_Code_Snippets/Task1_1_Model_Analysis.R")

cat("\nAI did one Analysis and Suggestion.\n",
    "You can either go along with it or switch\n",
    "if you believe, the AIs deduction is wrong\n",
    "1: GARCH (1,1) as Monte Carlo Simulation\n",
    "2: Linear/Random Walk as Monte Carlo Simulation\n")

modelselection <- readline(prompt = "Selection [1-2]: ")

switch(modelselection,
       "1" = {
         GARCH_Core <- TRUE
         message("Monte Carlo Simulation will use the Garch Multivariable Engine")
       },
       "2" = {
         LINEAR_Core <- TRUE
         message("Monte Carlo Simulation will use the Linear Multivariable Engine")
       },
       { # Default case (like Java's 'default')
         stop("Invalid selection. Script terminated.")
       }
)

# ==============================================================================
# UNIFIED SCENARIO SELECTOR
# ==============================================================================

message("initiating selector")

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
         base_done                 <- TRUE
         
         cat(paste(">>> Initializing Scenario:", mode_label, "\n"))
         
         message("Starting Base Simulation (used in Base Unhedge and Hedge Scenario)")
         
         if (exists("GARCH_Core")){ 
           source("Task_Code_Snippets/Task1_2_Simulation_GARCH.R")
         } else if (exists("LINEAR_Core")){ 
           source("Task_Code_Snippets/Task1_2_Simulation_LINEAR.R")
         } else {
           stop("No Clear Simulation Core selected! Stopping script")
         }
         
         message("Starting Base Hedge (used in Base Unhedge and Hedge Scenario)")
         source("Task_Code_Snippets/Task3_Exotic_Hedge.R")
         
         # This block automatically catches both "UNHEDGED" and "STRESSED UNHEDGED"
         message("Starting Base Unhedged calculation")
         source("Task_Code_Snippets/Task2_and_4_Calculation.R")
         
         message("Plotting NPV distribution (2b) and cumulative probability of distress (2d)")
         source("Task_Code_Snippets/Task2_Plot.R")
         
         
       },
       "2" = {
         ## implementing fail safe
         if(exists("base_done")) { 
           stress_test_mode          <- FALSE
           hedge_cashflow_calculator <- TRUE
           mode_label                <- "HEDGED"
           hedge_done                <- TRUE
           
           cat(paste(">>> Initializing Scenario:", mode_label, "\n"))
           
           message("Using past calculations from the Base Case (unhedged)")
           
           # This block automatically catches both "UNHEDGED" and "STRESSED UNHEDGED"
           message("Starting Base Hedge calculation")
           source("Task_Code_Snippets/Task2_and_4_Calculation.R")
           source("Task_Code_Snippets/Task4_Analysis.R")
           
           message("Testing optimal hedge levels")
           source("Task_Code_Snippets/Task4_optimal_hedge.R")
           
         } else {
           warning("Base case not executed ! Stopping Program")
         }
       },
       "3" = {
         ## implementing fail safe
         if(exists("base_done") && exists("hedge_done")) { 
           stress_test_mode          <- TRUE
           hedge_cashflow_calculator <- FALSE
           mode_label                <- "STRESSED UNHEDGED"
           
           
           cat(paste(">>> Initializing Scenario:", mode_label, "\n"))
           
           message("Starting Stressed Simulation (used in Stressed Unhedge Scenario)")
           
           if (exists("GARCH_Core")){ 
             source("Task_Code_Snippets/Task1_2_Simulation_GARCH.R")
           } else if (exists("LINEAR_Core")){ 
             source("Task_Code_Snippets/Task1_2_Simulation_LINEAR.R")
           } else {
             stop("No Clear Simulation Core selected! Stopping script")
           }
           
           message("Starting Base Hedge (used in Base Unhedge and Hedge Scenario)")
           source("Task_Code_Snippets/Task3_Exotic_Hedge.R")
           source("Task_Code_Snippets/Task2_and_4_Calculation.R")

           message("Performing Stress Test Analysis")
           source("Task_Code_Snippets/Task5_Stress_Analysis.R")
           
         } else {
           warning("Base case and/or hedged case not executed ! Stopping Program")
         }
       },
       { # Default case (like Java's 'default')
         stop("Invalid selection. Script terminated.")
       }
)
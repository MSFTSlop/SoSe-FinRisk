library(readxl)
library(moments) # Optional, or use: sum((ret_elec-mu)^4)/(length(ret_elec)*sigma^4) - 3 (Task 1_1)

## Manually set WDir to main.R file !!!

# ==============================================================================
# PROJECT: NordicWind SPV Financial Model
# PURPOSE: Master Data Preparation & Task 1 (Evolution Engine)
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. DATA INGESTION
# ------------------------------------------------------------------------------
# Linking the starting states and the transition factors
last_price_df     <- read_excel("data/DataPost_2026.xlsx", sheet = "Last_Price")
returns_df        <- read_excel("data/DataPost_2026.xlsx", sheet = "Returns")
logit_wind_ref_df <- read_excel("data/DataPost_2026.xlsx", sheet = "Logit_Wind")

## changed first column after import
colnames(last_price_df)[1]     <- "Date"
## readxl imported to many columns so i removed the unnecessary ones
returns_df <- returns_df[, -c(6:11)]

source("Task_Code_Snippets/Data_Prep.R")

# Task 1
message("Start model Choice engine")
source("Task_Code_Snippets/Task1_1_Model_Analysis.R")
message("Choice complete. Execute Modelling")
source("Task_Code_Snippets/Task1_2_Simulation.R")

# Task 2
message("Task 2 starting pure calculation")
source("Task_Code_Snippets/Task2_Calculation.R")
message("plotting npv distribution (2b) and cumulative probability of distress (2d)")
source("Task_Code_Snippets/Task2_Plot.R")

# Task 3
message("Start Task 3. The Hedge")
source("Task_Code_Snippets/Task3_Exotic_Hedge.R")

# Task 4

# Task 5

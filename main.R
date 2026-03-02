# install packages
install.packages("readxl") # CRAN version

# initialize packages
library(readxl)

## Manually set WDir to main.R file !!!

# prepare csv
last_price_df <- read_excel("data/DataPost_2026.xlsx", sheet = "Last_Price")
return_factors_df <- read_excel("data/DataPost_2026.xlsx", sheet = "Returns")
logit_wind_df <- read_excel("data/DataPost_2026.xlsx", sheet = "Logit_Wind")

colnames(last_price_df)[colnames(last_price_df) == '...1'] <- 'Date'

# Task 1

# Task 2

# Task 3
message("Start Task 3")
source("Task_Code_Snippets/Task3_Exotic_Hedge.R")

# Task 4

# Task 5

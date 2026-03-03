# 2. ANCHOR INITIALIZATION (T=0: January 2026)
# ------------------------------------------------------------------------------
# These variables represent the 'State of the World' at the start of the project.
# Every simulation path in Task 1 will 'evolve' from these coordinates.

# Market Prices (EUR/MWh or EUR/t)
p_elec_0 <- last_price_df$Electricity[1]   # 135.02
p_gas_0  <- last_price_df$NaturalGas[1]    # 75.705
p_carb_0 <- last_price_df$Carbon[1]        # 79.8

# Physical Wind Utilization (%)
u_wind_0 <- last_price_df$WindUtilization[1] # 0.3789

# 3. STATISTICAL INITIALIZATION (Logit-Space)
# ------------------------------------------------------------------------------
# Why? Utilization is a bounded percentage [0, 1]. To simulate it safely 
# without breaking physical limits (e.g. hitting 105%), we must start in 
# Logit-Space.

## seems as if gemini removed the logit table import and calculated its own
## logit wind vector

# Formula: L = ln( p / (1 - p) )

l_wind_0 <- log(u_wind_0 / (1 - u_wind_0))

# 4. PROJECT PARAMETERS (For Tasks 2-5)
# ------------------------------------------------------------------------------
# Defining constants as a list for easy referencing in downstream calculations.
project_params <- list(
  cap_wind        = 125,      # MW
  cap_gas         = 50,       # MW
  demand_ppa      = 50,       # MW (Data Center Obligation)
  hours_mo        = 720,      # Monthly operating hours (24*30)
  debt_principal  = 60e6,     # Bullet repayment at Month 60
  monthly_coupon  = 0.6e6,    # Interest expense
  distress_fee    = 0.10      # 10% penalty on equity injections
)
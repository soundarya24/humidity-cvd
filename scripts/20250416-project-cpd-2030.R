# import cpd-2021.csv from data folder

library(readr)
library(tidyverse)

# read here("output", "cost_per_daly_2021_relhum.rds")

cpd2021 <- readRDS(here::here(
  "output", "cost_per_daly_2021_relhum.rds"
  ))

# -----------------------------------------------
# Projecting Cost per DALY (CpD) for the year 2030
# -----------------------------------------------

# We assume a compound annual growth rate (CAGR) of 3% per year
# from 2021 to 2030 — a 9-year interval.

# Using the compound growth formula:
#   CpD_2030 = CpD_2021 × (1 + r)^n
# Where:
#   r = 0.03 (annual growth rate)
#   n = 9   (years from 2021 to 2030)

# So:
#   growth_multiplier = (1 + 0.03)^9 ≈ 1.305
# This implies: CpD in 2030 will be ~30.5% higher than CpD in 2021

# -----------------------------------------------
# Step 1: Define growth multiplier
# -----------------------------------------------
growth_multiplier <- (1 + 0.03)^9  # ≈ 1.305

# -----------------------------------------------
# Step 2: Apply projection to CpD_2021 values
# -----------------------------------------------
cpd2030 <- cpd2021 |> 
  mutate(CpD_2030 = cpd_2021 * growth_multiplier) |> 
  select(State, CpD_2030)

# Optional: Round off for presentation
cpd2030 <- cpd2030 |> 
  mutate(CpD_2030 = round(CpD_2030, 2))



# save as rds and csv
saveRDS(cpd2030, here::here(
  "output", "cpd-projections","cost_per_daly_2030_relhum.rds"
  ))
write_csv(cpd2030, here::here(
  "output","cpd-projections", "cost_per_daly_2030_relhum.csv"
  ))

# read csv file from cpd folder
# and read sheet name COST_PER_DALY_CVD

library(here)
library(readxl)

data_cost_per_daly_2021 <- read_excel(
  here("data", "CPD", "Statewise_Values.xlsx"), 
  sheet = "COST_PER_DALY_CVD"
) |> 
  janitor::clean_names()
names(data_cost_per_daly_2021)

data_cost_per_daly_2021 <- data_cost_per_daly_2021 |> 
  dplyr::rename(
    State=new_state,
    cpd_2021=cp_d_overall_cvd_2021
  ) |> 
  dplyr::select(
    State, cpd_2021
  ) |> 
  dplyr::mutate(
    cpd_2021 = as.numeric(cpd_2021)
  ) 

# check the data

data_cost_per_daly_2021 |> 
  dplyr::glimpse()

# write as rds
saveRDS(
  data_cost_per_daly_2021,
  here("output", "cost_per_daly_2021_relhum.rds")
)

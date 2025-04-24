pacman::p_load("forecast")

library(readr)
library(tidyverse)
library(readxl)  


gbd_data_uncorrected <- read_xlsx(here::here("data","gbd-uncorrected-april2025","CVD_DALY_YLL_YLD_2011-2019_Statewise_Uncorrected.xlsx")) |> 
  janitor::clean_names()
gbd_data_uncorrected <- gbd_data_uncorrected |> 
  dplyr::rename(State=location_2,
                Year=year_7,
                CVD_DALY=daly_val) |> 
  filter(Year %in% 2011:2021) |> 
  dplyr::select(Year,State,CVD_DALY)


names(gbd_data_uncorrected)



# Get unique states
states <- unique(gbd_data_uncorrected$State)

output_dir <- here::here("output","daly-projections", "state_forecasts")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Initialize an empty data frame to store all forecasts
all_forecasts <- data.frame()

# Loop over each state
for (state in states) {
  # Filter data for the current state
  state_data <- gbd_data_uncorrected |>
    filter(State == state) |>
    select(Year, CVD_DALY)
  
  # Create time series
  state_ts <- ts(state_data$CVD_DALY,
                 frequency = 1, start = c(2011, 1))
  
  # Fit ARIMA model
  fit_state <- auto.arima(state_ts)
  
  # Forecast next 11 years
  forecasted_values <- forecast(fit_state, 9)
  
  # Convert forecasted values to a data frame
  forecasted_df <- data.frame(
    State = state,
    Year = 2022:2030,
    Forecasted_CVD_DALY = as.numeric(forecasted_values$mean),
    Lower_80 = as.numeric(forecasted_values$lower[, 1]),
    Upper_80 = as.numeric(forecasted_values$upper[, 1]),
    Lower_95 = as.numeric(forecasted_values$lower[, 2]),
    Upper_95 = as.numeric(forecasted_values$upper[, 2])
  )
  
  # Append to the combined forecasts data frame
  all_forecasts <- bind_rows(all_forecasts, forecasted_df)
  
  # Optional: Save individual plots (already done earlier)
  # ...
}

# Write the combined forecast data frame to a CSV file
write_csv(all_forecasts, here::here("output", "daly-projections","combined_forecasts_daly_all_states.csv"))

# Print completion message
cat("All state forecasts have been combined and saved.\n")


all_forecasts <- read_csv(here::here("output", "daly-projections","combined_forecasts_daly_all_states.csv"))

# extract only 2030 data for everything

all_forecasts_2030 <- all_forecasts |>
  dplyr::filter(Year == 2030) |> 
  dplyr::select(State, Forecasted_CVD_DALY, Year) |> 
  mutate(Forecasted_CVD_DALY= round(Forecasted_CVD_DALY,0))


# for relative humidity
all_forecasts_2030 <- all_forecasts_2030 |>
  #dplyr::filter(Year == 2030) |>
  mutate(paf_daly_proj2030_avg975rh= round((Forecasted_CVD_DALY * 0.001),0),
         paf_daly_proj2030_avg25rh=round((Forecasted_CVD_DALY * 0.002), 0)) |>
  dplyr::rename(state = State)

# save as csv
write_csv(all_forecasts_2030, here::here("output", "daly-projections","absDALY2030_rh.csv"))
# save as rds
saveRDS(all_forecasts_2030, here::here("output", "daly-projections","absDALY2030_rh.rds"))

# for temperature

source(here::here("scripts", "20250416-function-paf-from-irr.R"))
calc_paf_from_irr(1.008164)
all_forecasts_2030_temp <- all_forecasts_2030 |>
  #dplyr::filter(Year == 2030) |>
  mutate(paf_daly_proj2030_avg975temp= round((Forecasted_CVD_DALY * 0.008),0)) 

# save as csv
write_csv(all_forecasts_2030_temp, here::here("output", "daly-projections","absDALY2030_temp.csv"))

# save as rds
saveRDS(all_forecasts_2030_temp, here::here("output", "daly-projections","absDALY2030_temp.rds"))

# for heatindex

source(here::here("scripts", "20250416-function-paf-from-irr.R"))
calc_paf_from_irr(1.014427)
all_forecasts_2030_hi <- all_forecasts_2030 |>
  #dplyr::filter(Year == 2030) |>
  mutate(paf_daly_proj2030_avg975hi= round((Forecasted_CVD_DALY * 0.014),0)) 

# save as csv
write_csv(all_forecasts_2030_hi, here::here("output", "daly-projections","absDALY2030_hi.csv"))

# save as rds
saveRDS(all_forecasts_2030_hi, here::here("output", "daly-projections","absDALY2030_hi.rds"))



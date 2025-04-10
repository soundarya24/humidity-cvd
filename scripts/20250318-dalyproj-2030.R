pacman::p_load("forecast")

library(readr)
library(tidyverse)
library(readxl)  
gbd_data_uncorrected <- read_excel(here::here("data","CVD_DALY_YLD_YLL_1990-2021_per100k.xlsx") )
View(gbd_data_uncorrected)
names(gbd_data_uncorrected)
gbd_data_uncorrected <- janitor::clean_names(gbd_data_uncorrected)

names(gbd_data_uncorrected)
# change variable names to State, Year and DALY
gbd_data_uncorrected <- gbd_data_uncorrected |>
  dplyr::rename(State = location, Year = year, CVD_DALY = cvd_daly) |> 
  dplyr::select(State, Year, CVD_DALY)



# Get unique states
states <- unique(gbd_data_uncorrected$State)

output_dir <- here::here("output","daly_proj", "state_forecasts")
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
  forecasted_values <- forecast(fit_state, 10)
  
  # Convert forecasted values to a data frame
  forecasted_df <- data.frame(
    State = state,
    Year = 2022:2031,
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
write_csv(all_forecasts, here::here("output", "daly_proj","combined_forecasts_daly_all_states.csv"))

# Print completion message
cat("All state forecasts have been combined and saved.\n")


all_forecasts <- read_csv(here::here("output", "daly_proj","combined_forecasts_daly_all_states.csv"))

# extract only 2031 data for everything

all_forecasts_2031 <- all_forecasts |>
  dplyr::filter(Year == 2031) |> 
  dplyr::select(State, Forecasted_CVD_DALY, Year) |> 
  mutate(Forecasted_CVD_DALY= round(Forecasted_CVD_DALY,0))

all_forecasts_2031 <- all_forecasts_2031 |>
  #dplyr::filter(Year == 2031) |>
  mutate(paf_daly= round((Forecasted_CVD_DALY * 0.001),0)) |>
  dplyr::rename(state = State)

write_csv(all_forecasts_2031, here::here("output", "daly_proj","daly_forecoasts_2031.csv"))


pacman::p_load("forecast")

library(readr)
library(tidyverse)
gbd_data_clean <- read_csv(here::here("data","gbd_updated_april24.csv"), 
                           show_col_types = FALSE)
gbd_data_clean <- gbd_data_clean |> 
  filter(Year %in% 2011:2019)

# Get unique states
states <- unique(gbd_data_clean$State)

output_dir <- here::here("output","daly_proj", "state_forecasts")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Initialize an empty data frame to store all forecasts
all_forecasts <- data.frame()

# Loop over each state
for (state in states) {
  # Filter data for the current state
  state_data <- gbd_data_clean |>
    filter(State == state) |>
    select(Year, CVD_DALY)
  
  # Create time series
  state_ts <- ts(state_data$CVD_DALY,
                 frequency = 1, start = c(2011, 1))
  
  # Fit ARIMA model
  fit_state <- auto.arima(state_ts)
  
  # Forecast next 11 years
  forecasted_values <- forecast(fit_state, 11)
  
  # Convert forecasted values to a data frame
  forecasted_df <- data.frame(
    State = state,
    Year = 2020:2030,
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

all_forecasts_2030 <- all_forecasts |>
  dplyr::filter(Year == 2030) |>
  mutate(paf= round((Forecasted_CVD_DALY * 0.001),0)) |>
  dplyr::rename(state = State)

## loop to try

# Load necessary libraries
pacman::p_load("forecast", "tidyverse", "zoo", "ggplot2", "scales")

# Read in datasets
combined_data_relhum_forpanel <- readRDS(here::here("data", "combined_data_relhum_forpanel_revised.rds"))
gbd_data_uncorrected <- read_csv(here::here("data", "gbd_data_uncorrected.csv"))
gbd_data_uncorrected <- gbd_data_uncorrected |>
  filter(Year %in% 2011:2019)

# Get unique states
states <- unique(gbd_data_uncorrected$State)

output_dir <- here::here("output","cvd_deaths", "state_forecasts")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}


## all forecast combi

# Initialize an empty data frame to store all forecasts
all_forecasts <- data.frame()

# Loop over each state
for (state in states) {
  # Filter data for the current state
  state_data <- gbd_data_uncorrected |>
    filter(State == state) |>
    select(Year, CVD_Death)
  
  # Create time series
  state_ts <- ts(state_data$CVD_Death,
                 frequency = 1, start = c(2011, 1))
  
  # Fit ARIMA model
  fit_state <- auto.arima(state_ts)
  
  # Forecast next 11 years
  forecasted_values <- forecast(fit_state, 11)
  
  # Convert forecasted values to a data frame
  forecasted_df <- data.frame(
    State = state,
    Year = 2020:2030,
    Forecasted_CVD_Deaths = as.numeric(forecasted_values$mean),
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
write_csv(all_forecasts, here::here("output", "cvd_deaths","combined_forecasts_all_states.csv"))

# Print completion message
cat("All state forecasts have been combined and saved.\n")


all_forecasts <- read_csv(here::here("output", "cvd_deaths","combined_forecasts_all_states.csv"))

all_forecasts_2030 <- all_forecasts |>
  dplyr::filter(Year == 2030) |>
  mutate(paf= round((Forecasted_CVD_Deaths * 0.002),0)) |>
  dplyr::rename(state = State)


library(sf)
library(tidyverse)

india <- readRDS(here::here("data",
                            "spatial_files",
                            "India_states.rds"))
india <- india |>
  rename(state = NAME_1) |>
  # recode orissa as odisha
  mutate(state = recode(state, "Orissa" = "Odisha")) |>
  # recode uttaranchal as uttarakhand
  mutate(state = recode(state, "Uttaranchal" = "Uttarakhand"))


# combine paf_data_formapviz with india based on state variable
india_paf <- india |>
  left_join(all_forecasts_2030, by = "state")
india_paf <- india_paf |>
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(x = st_coordinates(centroid)[,1],
         y = st_coordinates(centroid)[,2])



# mortality map
india_paf |>
  ggplot() +
  geom_sf(aes(fill = paf)) +
  scale_fill_gradient(low = "orange", high = "midnightblue") +
  # Add paf on the states as text with x and y coordinates
  geom_text(aes(x = x, y = y, label = paf), size = 3, color = "white") +
  labs(fill = "CVD deaths",
       title = "CVD deaths attributable to high average relative humidity in Indian states",
       subtitle = "Year 2030") +
  theme_classic() +
  # Make x-axis and y-axis blank
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())





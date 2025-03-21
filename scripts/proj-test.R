library(readr)
library(tidyverse)
library(readxl) 
# import csv data
data_proj_test <- read_csv(here::here(
  "data", "StatewiseDALYTrend.csv"
))
names(data_proj_test)
# wide to long data
data_proj_test_long <- data_proj_test %>% 
  pivot_longer(cols = -year, names_to = "State", values_to = "DALY")


data_proj_test_long_clean <- data_proj_test_long %>%
  mutate(State = str_remove(State, "^DALY-"))

all_forecasts_2030 <- read_csv(here::here("output", "daly_proj","daly_forecoasts_2030.csv"))
# change state to State
all_forecasts_2030 <- all_forecasts_2030 |>
  dplyr::rename(State = state)

# change Year to year, state to State and DALY
all_forecasts_2030_tomerge <- all_forecasts_2030 |>
  dplyr::rename(year = Year, DALY = Forecasted_CVD_DALY) |> 
  dplyr::select(-paf)
# merge data_proj_test_long_clean and all_forecasts_2030_tomerge

daly_combined <- bind_rows(data_proj_test_long_clean, all_forecasts_2030_tomerge)

# produce line charts please with direct labels
library(directlabels)

daly_combined |>
  ggplot(aes(x = year, y = DALY, color = State)) +
  geom_line() +
  ylim(0,max(daly_combined$DALY))+ 
  geom_dl(aes(label = State), method = list("last.points")) +
  labs(title = "DALY by State", x = "Year", y = "DALY") +
  theme_minimal()+
  theme(legend.position = "none")


# Add "DALY-" prefix to state names
all_forecasts_2030_tobackmerge <- all_forecasts_2030_tomerge %>%
  mutate(State = str_c("DALY-", State))

# Step 3: Pivot to wide format (states as columns)
all_forecasts_2030_tobackmerge_wide <- all_forecasts_2030_tobackmerge %>%
  select(State, year, DALY) %>%
  pivot_wider(names_from = State, values_from = DALY)

write.csv(all_forecasts_2030_tobackmerge_wide, here::here("output", "daly_proj", "daly_forecoasts_2030_wide.csv"))
          
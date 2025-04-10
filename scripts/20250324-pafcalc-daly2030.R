### PAF

library(readr)
library(tidyverse)
daly_data_projected <- read_csv(here::here("output","daly_proj","combined_forecasts_daly_all_states.csv"))
daly_data_projected <- daly_data_projected |> 
  dplyr::select(Year,State,Forecasted_CVD_DALY)

combined_data_relhum_forpanel <- readRDS(here::here("data","combined_data_relhum_forpanel_revised.rds"))


# rename state.name as State
combined_data_relhum_forpanel <- combined_data_relhum_forpanel |> 
dplyr::rename(State = state_name,
            Year = year)


paf_data_formapviz_economy_2030 <- daly_data_projected |>
  dplyr::select(State, Year, Forecasted_CVD_DALY) |>
  filter(Year==2030) |> 
  mutate(paf_daly= round((Forecasted_CVD_DALY * 0.001))
         #,
         #paf_yld= round(YLD * 0.002),
         #paf_yll=round(YLL * 0.001)
         ) 

# save as rds and csv
saveRDS(paf_data_formapviz_economy_2030, here::here("data","paf_data_formapviz_economy_2030.rds"))
write_csv(paf_data_formapviz_economy_2030, here::here("data","paf_data_formapviz_economy_2030.csv"))





#################################################
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
india_paf_economy <- india |> 
  left_join(paf_data_formapviz_economy, by = "state")
india_paf_economy <- india_paf_economy |> 
  mutate(centroid = st_centroid(geometry)) %>% 
  mutate(x = st_coordinates(centroid)[,1], 
         y = st_coordinates(centroid)[,2])


# daly map
india_paf_economy |> 
  ggplot() +
  geom_sf(aes(fill = paf_daly)) +
  scale_fill_gradient(low = "orange", high = "midnightblue") +
  # Add paf on the states as text with x and y coordinates
  geom_text(aes(x = x, y = y, label = paf), size = 3, color = "white") +
  labs(fill = "CVD DALY",
       title = "CVD DALY attributable to high average humidity in Indian states",
       subtitle = "Year 2019") +
  theme_classic() +
  # Make x-axis and y-axis blank
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())

# yld map
india_paf_economy |> 
  ggplot() +
  geom_sf(aes(fill = paf_yld)) +
  scale_fill_gradient(low = "orange", high = "midnightblue") +
  # Add paf on the states as text with x and y coordinates
  geom_text(aes(x = x, y = y, label = paf_incid), size = 3, color = "white") +
  labs(fill = "CVD YLD",
       title = "CVD YLD attributable to high average humidity in Indian states",
       subtitle = "Year 2019") +
  theme_classic() +
  # Make x-axis and y-axis blank
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())


# yll map
india_paf |> 
  ggplot() +
  geom_sf(aes(fill = paf_yll)) +
  scale_fill_gradient(low = "orange", high = "midnightblue",
                      labels=scales::comma) +
  # Add paf on the states as text with x and y coordinates
  geom_text(aes(x = x, y = y, label = paf_prev), size = 3, color = "white") +
  labs(fill = "CVD YLL",
       title = "CVD YLL attributable to high average humidity in Indian states",
       subtitle = "Year 2019") +
  theme_classic() +
  # Make x-axis and y-axis blank
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())


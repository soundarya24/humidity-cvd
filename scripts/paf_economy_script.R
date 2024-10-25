### PAF


#| label: pafcalc-economy
#| results: hide
#| echo: false
#| warning: false
#| message: false
gbd_data_uncorrected <- read_csv(here::here("data","gbd_data_uncorrected.csv"))
gbd_data_uncorrected <- gbd_data_uncorrected |> 
  filter(Year %in% 2011:2019)

# rename state.name as State
#combined_data_temperature_forpanel 
#<- combined_data_temperature_forpanel |> 
#dplyr::rename(State = state_name,
#             Year = year)

# merge temperature data and gbd data

combined_data_uncorrectedgbd <- merge(
  combined_data_relhum_forpanel,
  gbd_data_uncorrected,
  by = c("State", "Year")
)

paf_data_formapviz_economy <- combined_data_uncorrectedgbd |>
  dplyr::select(State, Year, CVD_DALY, avg_975,
                CVD_YLD, CVD_YLL) |>
  filter(Year==2019) |> 
  mutate(paf_daly= round((CVD_DALY * 0.001)),
         paf_yld= round(CVD_YLD * 0.002),
         paf_yll=round(CVD_YLL * 0.001)) |> 
  dplyr::select(-CVD_DALY,-avg_975,-CVD_YLD,-CVD_YLL) |> 
  #rename State as state
  dplyr::rename(state = State)


#| label: fig-pafmap-economy
#| echo: false
#| layout-ncol: 1
#| fig-cap: CVD deaths attributable to high average temperature in Indian states
#| fig-subcap: 
#|     - CVD Mortality
#|     - CVD Incidence
#|     - CVD Prevalence
#| fig-height: 10
#| fig-width: 14
#| fig-align: center
#| fig-cap-location: top
#| message: false
#| warning: false
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


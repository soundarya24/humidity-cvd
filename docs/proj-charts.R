## Projection maps for 2.5th percentile of average relative humidity

### CVD Deaths

```{r}
#| label: projmaps-deaths-lowaverage
#| warning: false
#| message: false

all_forecasts_deaths <- read_csv(here::here("output", "cvd_deaths","combined_forecasts_all_states.csv"))

all_forecasts_2030_deaths <- all_forecasts_deaths |>
  dplyr::filter(Year == 2030) |>
  mutate(paf_deaths= round((Forecasted_CVD_Deaths * 0.0099),0)) |>
  dplyr::rename(state = State)

rmarkdown::paged_table(all_forecasts_2030_deaths)

```

```{r}
#| label: fig-projmaps-deaths-lowaverage
#| echo: true
#| layout-ncol: 1
#| fig-height: 10
#| fig-width: 14
#| fig-align: center
#| fig-cap-location: top
#| message: false
#| warning: false
#| column: screen
#| out-width: 100%
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
india_paf_deaths <- india |>
  left_join(all_forecasts_2030_deaths, by = "state")
india_paf_deaths <- india_paf_deaths |>
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(x = st_coordinates(centroid)[,1],
         y = st_coordinates(centroid)[,2])



# mortality map
india_paf_deaths |>
  ggplot() +
  geom_sf(aes(fill = paf_deaths)) +
  scale_fill_gradient(low = "orange", high = "midnightblue") +
  # Add paf on the states as text with x and y coordinates
  geom_text(aes(x = x, y = y, label = paf_deaths), size = 3, color = "white") +
  labs(fill = "CVD deaths",
       title = "CVD deaths attributable to low average relative humidity in Indian states",
       subtitle = "Year 2030") +
  theme_classic() +
  # Make x-axis and y-axis blank
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())

```

### CVD Prevalence

```{r}
#| label: projmaps-prev-lowaverage
#| warning: false
#| message: false


all_forecasts_prev <- read_csv(here::here("output", "cvd_prev","combined_forecasts_all_states.csv"))

all_forecasts_2030_prev <- all_forecasts_prev |>
  dplyr::filter(Year == 2030) |>
  mutate(paf_prev= round((Forecasted_CVD_Prevalences * 0.0157),0)) |>
  dplyr::rename(state = State)

rmarkdown::paged_table(all_forecasts_2030_prev)
```

```{r}
#| label: fig-projmaps-prev-lowaverage
#| echo: true
#| layout-ncol: 1
#| fig-height: 10
#| fig-width: 14
#| fig-align: center
#| fig-cap-location: top
#| message: false
#| warning: false
#| column: screen
#| out-width: 100%


# combine paf_data_formapviz with india based on state variable
india_paf_prev <- india |>
  left_join(all_forecasts_2030_prev, by = "state")
india_paf_prev <- india_paf_prev |>
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(x = st_coordinates(centroid)[,1],
         y = st_coordinates(centroid)[,2])



# prevalence map
india_paf_prev |>
  ggplot() +
  geom_sf(aes(fill = paf_prev)) +
  scale_fill_gradient(low = "orange", high = "midnightblue") +
  # Add paf on the states as text with x and y coordinates
  geom_text(aes(x = x, y = y, label = paf_prev), size = 3, color = "white") +
  labs(fill = "CVD prevalence",
       title = "CVD prevalence attributable to low average relative humidity in Indian states",
       subtitle = "Year 2030") +
  theme_classic() +
  # Make x-axis and y-axis blank
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())





```

### CVD Incidence

```{r}
#| label: projmaps-inci-lowaverage
#| warning: false
#| message: false

all_forecasts_incidence <- read_csv(here::here("output", "cvd_inci","combined_forecasts_all_states.csv"))

all_forecasts_2030_incidence <- all_forecasts_incidence |>
  dplyr::filter(Year == 2030) |>
  mutate(paf_inci= round((Forecasted_CVD_Incidences * 0.0128),0)) |>
  dplyr::rename(state = State)

rmarkdown::paged_table(all_forecasts_2030_incidence)
```

```{r}
#| label: fig-projmaps-inci-lowaverage
#| echo: true
#| layout-ncol: 1
#| fig-height: 10
#| fig-width: 14
#| fig-align: center
#| fig-cap-location: top
#| message: false
#| warning: false
#| column: screen
#| out-width: 100%


# combine paf_data_formapviz with india based on state variable
india_paf_inci <- india |>
  left_join(all_forecasts_2030_incidence, by = "state")
india_paf_inci <- india_paf_inci |>
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(x = st_coordinates(centroid)[,1],
         y = st_coordinates(centroid)[,2])



# incidence map
india_paf_inci |>
  ggplot() +
  geom_sf(aes(fill = paf_inci)) +
  scale_fill_gradient(low = "orange", high = "midnightblue") +
  # Add paf on the states as text with x and y coordinates
  geom_text(aes(x = x, y = y, label = paf_inci), size = 3, color = "white") +
  labs(fill = "CVD incidence",
       title = "CVD incidence attributable to low average relative humidity in Indian states",
       subtitle = "Year 2030") +
  theme_classic() +
  # Make x-axis and y-axis blank
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())



```

## Projection maps for 2.5th percentile of variation in relative humidity

### CVD Deaths

```{r}
#| label: projmaps-deaths-lowvariation
#| warning: false
#| message: false

all_forecasts_deaths <- read_csv(here::here("output", "cvd_deaths","combined_forecasts_all_states.csv"))

all_forecasts_2030_deaths <- all_forecasts_deaths |>
  dplyr::filter(Year == 2030) |>
  mutate(paf_deaths= round((Forecasted_CVD_Deaths * 0.0138),0)) |>
  dplyr::rename(state = State)

rmarkdown::paged_table(all_forecasts_2030_deaths)

```

```{r}
#| label: fig-projmaps-deaths-lowvariation
#| echo: true
#| layout-ncol: 1
#| fig-height: 10
#| fig-width: 14
#| fig-align: center
#| fig-cap-location: top
#| message: false
#| warning: false
#| column: screen
#| out-width: 100%
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
india_paf_deaths <- india |>
  left_join(all_forecasts_2030_deaths, by = "state")
india_paf_deaths <- india_paf_deaths |>
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(x = st_coordinates(centroid)[,1],
         y = st_coordinates(centroid)[,2])



# mortality map
india_paf_deaths |>
  ggplot() +
  geom_sf(aes(fill = paf_deaths)) +
  scale_fill_gradient(low = "orange", high = "midnightblue") +
  # Add paf on the states as text with x and y coordinates
  geom_text(aes(x = x, y = y, label = paf_deaths), size = 3, color = "white") +
  labs(fill = "CVD deaths",
       title = "CVD deaths attributable to low variation in relative humidity in Indian states",
       subtitle = "Year 2030") +
  theme_classic() +
  # Make x-axis and y-axis blank
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())

```

### CVD Prevalence

```{r}
#| label: projmaps-prev-lowvariation
#| warning: false
#| message: false


all_forecasts_prev <- read_csv(here::here("output", "cvd_prev","combined_forecasts_all_states.csv"))

all_forecasts_2030_prev <- all_forecasts_prev |>
  dplyr::filter(Year == 2030) |>
  mutate(paf_prev= round((Forecasted_CVD_Prevalences * 0.0138),0)) |>
  dplyr::rename(state = State)

rmarkdown::paged_table(all_forecasts_2030_prev)
```

```{r}
#| label: fig-projmaps-prev-lowvariation
#| echo: true
#| layout-ncol: 1
#| fig-height: 10
#| fig-width: 14
#| fig-align: center
#| fig-cap-location: top
#| message: false
#| warning: false
#| column: screen
#| out-width: 100%


# combine paf_data_formapviz with india based on state variable
india_paf_prev <- india |>
  left_join(all_forecasts_2030_prev, by = "state")
india_paf_prev <- india_paf_prev |>
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(x = st_coordinates(centroid)[,1],
         y = st_coordinates(centroid)[,2])



# prevalence map
india_paf_prev |>
  ggplot() +
  geom_sf(aes(fill = paf_prev)) +
  scale_fill_gradient(low = "orange", high = "midnightblue") +
  # Add paf on the states as text with x and y coordinates
  geom_text(aes(x = x, y = y, label = paf_prev), size = 3, color = "white") +
  labs(fill = "CVD prevalence",
       title = "CVD prevalence attributable to low variation in relative humidity in Indian states",
       subtitle = "Year 2030") +
  theme_classic() +
  # Make x-axis and y-axis blank
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())





```

### CVD Incidence

```{r}
#| label: projmaps-inci-lowvariation
#| warning: false
#| message: false

all_forecasts_incidence <- read_csv(here::here("output", "cvd_inci","combined_forecasts_all_states.csv"))

all_forecasts_2030_incidence <- all_forecasts_incidence |>
  dplyr::filter(Year == 2030) |>
  mutate(paf_inci= round((Forecasted_CVD_Incidences * 0.01088),0)) |>
  dplyr::rename(state = State)

rmarkdown::paged_table(all_forecasts_2030_incidence)
```

```{r}
#| label: fig-projmaps-inci-lowvariation
#| echo: true
#| layout-ncol: 1
#| fig-height: 10
#| fig-width: 14
#| fig-align: center
#| fig-cap-location: top
#| message: false
#| warning: false
#| column: screen
#| out-width: 100%


# combine paf_data_formapviz with india based on state variable
india_paf_inci <- india |>
  left_join(all_forecasts_2030_incidence, by = "state")
india_paf_inci <- india_paf_inci |>
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(x = st_coordinates(centroid)[,1],
         y = st_coordinates(centroid)[,2])



# incidence map
india_paf_inci |>
  ggplot() +
  geom_sf(aes(fill = paf_inci)) +
  scale_fill_gradient(low = "orange", high = "midnightblue") +
  # Add paf on the states as text with x and y coordinates
  geom_text(aes(x = x, y = y, label = paf_inci), size = 3, color = "white") +
  labs(fill = "CVD incidence",
       title = "CVD incidence attributable to low variation in relative humidity in Indian states",
       subtitle = "Year 2030") +
  theme_classic() +
  # Make x-axis and y-axis blank
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())



```
## Projection maps for 97.5th percentile of average relative humidity

### CVD Deaths

```{r}
#| label: projmaps-deaths
#| warning: false
#| message: false

all_forecasts_deaths <- read_csv(here::here("output", "cvd_deaths","combined_forecasts_all_states.csv"))

all_forecasts_2030_deaths <- all_forecasts_deaths |>
  dplyr::filter(Year == 2030) |>
  mutate(paf= round((Forecasted_CVD_Deaths * 0.002),0)) |>
  dplyr::rename(state = State)

rmarkdown::paged_table(all_forecasts_2030_deaths)

```

```{r}
#| label: fig-projmaps-deaths
#| echo: true
#| layout-ncol: 1
#| fig-height: 10
#| fig-width: 14
#| fig-align: center
#| fig-cap-location: top
#| message: false
#| warning: false
#| column: screen
#| out-width: 100%
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
india_paf_deaths <- india |>
  left_join(all_forecasts_2030_deaths, by = "state")
india_paf_deaths <- india_paf_deaths |>
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(x = st_coordinates(centroid)[,1],
         y = st_coordinates(centroid)[,2])



# mortality map
india_paf_deaths |>
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

```

### CVD Prevalence

```{r}
#| label: projmaps-prev
#| warning: false
#| message: false


all_forecasts_prev <- read_csv(here::here("output", "cvd_prev","combined_forecasts_all_states.csv"))

all_forecasts_2030_prev <- all_forecasts_prev |>
  dplyr::filter(Year == 2030) |>
  mutate(paf= round((Forecasted_CVD_Prevalences * 0.002),0)) |>
  dplyr::rename(state = State)

rmarkdown::paged_table(all_forecasts_2030_prev)
```

```{r}
#| label: fig-projmaps-prev
#| echo: true
#| layout-ncol: 1
#| fig-height: 10
#| fig-width: 14
#| fig-align: center
#| fig-cap-location: top
#| message: false
#| warning: false
#| column: screen
#| out-width: 100%


# combine paf_data_formapviz with india based on state variable
india_paf_prev <- india |>
  left_join(all_forecasts_2030_prev, by = "state")
india_paf_prev <- india_paf_prev |>
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(x = st_coordinates(centroid)[,1],
         y = st_coordinates(centroid)[,2])



# prevalence map
india_paf_prev |>
  ggplot() +
  geom_sf(aes(fill = paf)) +
  scale_fill_gradient(low = "orange", high = "midnightblue") +
  # Add paf on the states as text with x and y coordinates
  geom_text(aes(x = x, y = y, label = paf), size = 3, color = "white") +
  labs(fill = "CVD prevalence",
       title = "CVD prevalence attributable to high average relative humidity in Indian states",
       subtitle = "Year 2030") +
  theme_classic() +
  # Make x-axis and y-axis blank
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())





```

### CVD Incidence

```{r}
#| label: projmaps-inci
#| warning: false
#| message: false

all_forecasts_incidence <- read_csv(here::here("output", "cvd_inci","combined_forecasts_all_states.csv"))

all_forecasts_2030_incidence <- all_forecasts_incidence |>
  dplyr::filter(Year == 2030) |>
  mutate(paf= round((Forecasted_CVD_Incidences * 0.002),0)) |>
  dplyr::rename(state = State)

rmarkdown::paged_table(all_forecasts_2030_incidence)
```

```{r}
#| label: fig-projmaps-inci
#| echo: true
#| layout-ncol: 1
#| fig-height: 10
#| fig-width: 14
#| fig-align: center
#| fig-cap-location: top
#| message: false
#| warning: false
#| column: screen
#| out-width: 100%


# combine paf_data_formapviz with india based on state variable
india_paf_inci <- india |>
  left_join(all_forecasts_2030_incidence, by = "state")
india_paf_inci <- india_paf_inci |>
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(x = st_coordinates(centroid)[,1],
         y = st_coordinates(centroid)[,2])



# incidence map
india_paf_inci |>
  ggplot() +
  geom_sf(aes(fill = paf)) +
  scale_fill_gradient(low = "orange", high = "midnightblue") +
  # Add paf on the states as text with x and y coordinates
  geom_text(aes(x = x, y = y, label = paf), size = 3, color = "white") +
  labs(fill = "CVD incidence",
       title = "CVD incidence attributable to high average relative humidity in Indian states",
       subtitle = "Year 2030") +
  theme_classic() +
  # Make x-axis and y-axis blank
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())



```

### IRR - cvd prevalence incidence mortality

#| Humidity | Percentile | Mortality \[95% CIs\] | Prevalence\[95% CIs\] | Incidence\[95% CIs\] |
# |---------------|---------------|---------------|---------------|---------------|
# | Average | 2.5 | **`r round(irr_avg25_death,3)` \[`r round(irr_lci_avg25_death,3)`,`r round(irr_uci_avg25_death,3)`\]** | **`r round(irr_avg25_prevalence,3)`\[`r round(irr_lci_avg25_prevalence,3)`,`r round(irr_uci_avg25_prevalence,3)`\]** | **`r round(irr_avg25_incidence,3)`\[`r round(irr_lci_avg25_incidence,3)`,`r round(irr_uci_avg25_incidence,3)`\]** |
# | Average | 97.5 | **`r round(irr_avg975_death,3)` \[`r round(irr_lci_avg975_death,3)`,`r round(irr_uci_avg975_death,3)`\]** | **`r round(irr_avg975_prevalence,3)`\[`r round(irr_lci_avg975_prevalence,3)`,`r round(irr_uci_avg975_prevalence,3)`\]** | **`r round(irr_avg975_incidence,3)`\[`r round(irr_lci_avg975_incidence,3)`,`r round(irr_uci_avg975_incidence,3)`\]** |
  
# : Incidence Rate Ratios derived from Poisson panel regressions for CVD burden (mortality, prevalence, and incidence) {#tbl-my-table}
    
#   ------------------------------------------------------------------------
      
      
      
      # yld map
      india_paf_economy |> 
      ggplot() +
      geom_sf(aes(fill = paf_yld)) +
      scale_fill_gradient(low = "orange", high = "midnightblue") +
      # Add paf on the states as text with x and y coordinates
      geom_text(aes(x = x, y = y, label = paf_yld), size = 3, color = "white") +
      labs(fill = "CVD YLD",
           title = "CVD YLD attributable to high average humidity in Indian states",
           subtitle = "Year 2021") +
      theme_classic() +
      # Make x-axis and y-axis blank
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank())
    
    
    # YLL map
    india_paf_economy |> 
      ggplot() +
      geom_sf(aes(fill = paf_yll)) +
      scale_fill_gradient(low = "orange", high = "midnightblue",
                          labels=scales::comma) +
      # Add paf on the states as text with x and y coordinates
      geom_text(aes(x = x, y = y, label = paf_yll), size = 3, color = "white") +
      labs(fill = "CVD YLL",
           title = "CVD YLL attributable to high average humidity in Indian states",
           subtitle = "Year 2021") +
      theme_classic() +
      # Make x-axis and y-axis blank
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank())
    
    
    ### PAF
    
    ```{r}
    #| label: pafcalc
    #| results: hide
    #| echo: false
    #| warning: false
    #| message: false
    gbd_data_uncorrected_forprev <- read_csv(here::here("data","gbd_data_uncorrected.csv"))
    
    
    gbd_data_uncorrected_forprev <- gbd_data_uncorrected_forprev |> 
      dplyr::filter(Year %in% 2011:2021) |> 
      dplyr::select(State,Year,starts_with("CVD_"))
    
    # rename state.name as State
    #combined_data_temperature_forpanel 
    #<- combined_data_temperature_forpanel |> 
    #dplyr::rename(State = state_name,
    #             Year = year)
    
    # merge temperature data and gbd data
    combined_data_relhum_forpanel <- combined_data_relhum_forpanel |> 
      dplyr::filter(Year %in% 2011:2021)
    combined_data_uncorrectedgbd <- merge(
      combined_data_relhum_forpanel,
      gbd_data_uncorrected_forprev,
      by = c("State", "Year")
    )
    
    paf_data_formapviz <- combined_data_uncorrectedgbd |>
      dplyr::select(State, Year, CVD_Death, avg_975,
                    CVD_Prevalence, CVD_Incidence) |>
      dplyr::filter(Year==2019) |> 
      mutate(paf= round((CVD_Death * 0.002)),
             paf_prev= round(CVD_Prevalence * 0.002),
             paf_incid=round(CVD_Incidence * 0.002)) |> 
      dplyr::select(-CVD_Death,-avg_975) |> 
      #rename State as state
      dplyr::rename(state = State)
    
    
    ```
    
    ```{r}
    #| label: fig-pafmap
    #| results: hide
    #| echo: false
    #| layout-ncol: 1
    #| fig-cap: CVD deaths attributable to high average humidity in Indian states
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
    #| column: screen
    #| out-width: 100%
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
      left_join(paf_data_formapviz, by = "state")
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
           title = "CVD deaths attributable to high average humidity in Indian states",
           subtitle = "Year 2019") +
      theme_classic() +
      # Make x-axis and y-axis blank
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank())
    
    # Incidence map
    india_paf |> 
      ggplot() +
      geom_sf(aes(fill = paf_incid)) +
      scale_fill_gradient(low = "orange", high = "midnightblue") +
      # Add paf on the states as text with x and y coordinates
      geom_text(aes(x = x, y = y, label = paf_incid), size = 3, color = "white") +
      labs(fill = "CVD Incidence",
           title = "CVD Incidence attributable to high average humidity in Indian states",
           subtitle = "Year 2019") +
      theme_classic() +
      # Make x-axis and y-axis blank
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank())
    
    
    # Prevalence map
    india_paf |> 
      ggplot() +
      geom_sf(aes(fill = paf_prev)) +
      scale_fill_gradient(low = "orange", high = "midnightblue",
                          labels=scales::comma) +
      # Add paf on the states as text with x and y coordinates
      geom_text(aes(x = x, y = y, label = paf_prev), size = 3, color = "white") +
      labs(fill = "CVD Prevalence",
           title = "CVD Prevalence attributable to high average humidity in Indian states",
           subtitle = "Year 2019") +
      theme_classic() +
      # Make x-axis and y-axis blank
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank())
    
    ```
    
    ------------------------------------------------------------------------
      
      
      ```{r}
    #| label: panelregression
    #| results: hide
    #| echo: false
    #| warning: false
    #| message: false
    model_CVD_Death_avg25 <- pglm(CVD_Death ~ average_25,
                                  data = combined_data,
                                  family = poisson,
                                  model = "within",
                                  index = c("State", "Year"))
    summary(model_CVD_Death_avg25)
    confint(model_CVD_Death_avg25)[, 1]
    confint(model_CVD_Death_avg25)[, 2]
    irr_avg25_death <- exp(coef(model_CVD_Death_avg25))
    irr_avg25_death
    irr_lci_avg25_death <- exp(confint(model_CVD_Death_avg25)[, 1])
    irr_uci_avg25_death <- exp(confint(model_CVD_Death_avg25)[, 2])
    irr_lci_avg25_death
    irr_uci_avg25_death
    
    model_CVD_Death_avg975 <- pglm(CVD_Death ~ average_975,
                                   data = combined_data,
                                   family = poisson,
                                   model = "within",
                                   index = c("State", "Year"))
    summary(model_CVD_Death_avg975)
    confint(model_CVD_Death_avg975)[, 1]
    confint(model_CVD_Death_avg975)[, 2]
    irr_avg975_death <- exp(coef(model_CVD_Death_avg975))
    irr_avg975_death
    irr_lci_avg975_death <- exp(confint(model_CVD_Death_avg975)[, 1])
    irr_uci_avg975_death <- exp(confint(model_CVD_Death_avg975)[, 2])
    irr_lci_avg975_death
    irr_uci_avg975_death
    
    
    ### CVD_Incidence
    
    model_CVD_Incidence_avg25 <- pglm(CVD_Incidence ~ average_25,
                                      data = combined_data,
                                      family = poisson,
                                      model = "within",
                                      index = c("State", "Year"))
    summary(model_CVD_Incidence_avg25)
    irr_avg25_incidence <- exp(coef(model_CVD_Incidence_avg25))
    irr_avg25_incidence
    irr_lci_avg25_incidence <- exp(confint(model_CVD_Incidence_avg25)[, 1])
    irr_uci_avg25_incidence <- exp(confint(model_CVD_Incidence_avg25)[, 2])
    irr_lci_avg25_incidence
    irr_uci_avg25_incidence
    
    
    
    model_CVD_Incidence_avg975 <- pglm(CVD_Incidence ~ average_975,
                                       data = combined_data,
                                       family = poisson,
                                       model = "within",
                                       index = c("State", "Year"))
    summary(model_CVD_Incidence_avg975)
    irr_avg975_incidence <- exp(coef(model_CVD_Incidence_avg975))
    irr_avg975_incidence
    irr_lci_avg975_incidence <- exp(confint(model_CVD_Incidence_avg975)[, 1])
    irr_uci_avg975_incidence <- exp(confint(model_CVD_Incidence_avg975)[, 2])
    irr_lci_avg975_incidence
    irr_uci_avg975_incidence
    
    
    
    ### CVD_Prevalence
    
    model_CVD_Prevalence_avg25 <- pglm(CVD_Prevalence ~ average_25,
                                       data = combined_data,
                                       family = poisson,
                                       model = "within",
                                       index = c("State", "Year"))
    summary(model_CVD_Prevalence_avg25)
    irr_avg25_prevalence <- exp(coef(model_CVD_Prevalence_avg25))
    irr_avg25_prevalence
    irr_lci_avg25_prevalence <- exp(confint(model_CVD_Prevalence_avg25)[, 1])
    irr_uci_avg25_prevalence <- exp(confint(model_CVD_Prevalence_avg25)[, 2])
    irr_lci_avg25_prevalence
    irr_uci_avg25_prevalence
    
    
    
    model_CVD_Prevalence_avg975 <- pglm(CVD_Prevalence ~ average_975,
                                        data = combined_data,
                                        family = poisson,
                                        model = "within",
                                        index = c("State", "Year"))
    summary(model_CVD_Prevalence_avg975)
    irr_avg975_prevalence <- exp(coef(model_CVD_Prevalence_avg975))
    irr_avg975_prevalence
    irr_lci_avg975_prevalence <- exp(confint(model_CVD_Prevalence_avg975)[, 1])
    irr_uci_avg975_prevalence <- exp(confint(model_CVD_Prevalence_avg975)[, 2])
    irr_lci_avg975_prevalence
    irr_uci_avg975_prevalence
    
    
    
    
    ```
    
    
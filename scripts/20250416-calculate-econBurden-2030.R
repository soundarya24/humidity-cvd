# -------------------------------------------------------------------------
# Estimate Projected Economic Burden Attributable to High Relative Humidity
# -------------------------------------------------------------------------

# This section performs the following key operations:
#
# 1. Projects DALYs for cardiovascular diseases (CVD) in 2030 using ARIMA 
#    models based on GBD data, with DALYs_2021 as the latest input.
#
# 2. Projects Cost per DALY (CpD) for 2030 using compound inflation-based 
#    growth from CpD_2021. The projected CpD_2030 accounts for expected 
#    economic inflation over the 9-year period (2021–2030).
#
# 3. Calculates population-attributable DALYs due to high relative humidity:
#    PAF × DALY_2030 = DALYs attributable to high RH in 2030.
#
# 4. Multiplies the attributable DALYs by CpD_2030 to obtain the projected 
#    economic burden in INR for each state:
#    Economic_Burden_2030 = Attributable_DALYs_2030 × CpD_2030
#
# The result is a forward-looking estimate of climate-linked health costs 
# related to high relative humidity, aligned to 2030 forecasts.

# import cpd proj data from folder output, cpd-projections, file name cost_per_daly_2030_relhum.rds using here package

cpd2030 <- readRDS(here::here(
  "output", "cpd-projections", "cost_per_daly_2030_relhum.rds"
  ))

# import daly proj data from output, daly-projections, filename absDALY2030_rh.rds

daly_2030 <- readRDS(here::here(
  "output", "daly-projections", "absDALY2030_rh.rds"
  )) 

daly_2030 <- daly_2030 |> 
 dplyr::rename(State=state)

# join both by State

daly_2030_cpd2030 <- daly_2030 |> 
  left_join(cpd2030, by = "State")
names(daly_2030_cpd2030)

daly_totalburden_cvd_2030 <- daly_2030_cpd2030 |> 
  dplyr::mutate(
    totaleconBurden_2030=paf_daly_proj2030*CpD_2030
  )



# save as rds and csv
saveRDS(daly_totalburden_cvd_2030, here::here("output","total-econ-burden-projections","rh_daly_totalburden_cvd_2030.rds"))
write_csv(daly_totalburden_cvd_2030, here::here("output","total-econ-burden-projections","rh_daly_totalburden_cvd_2030.csv"))  


# bar charts
daly_totalburden_cvd_2030 |> 
  drop_na() |>
  ggplot(aes(x=reorder(State, totaleconBurden_2030), y=totaleconBurden_2030/10000000))+
  geom_bar(stat="identity")+
  coord_flip()+
  labs(title="Projected Economic burden of CVD in India in 2030 due to high humidity",
       x="State",
       y="Economic burden in crores")+
  theme_minimal()+
  # using scales add crores in y axis as suffix,
  scale_y_continuous(labels = scales::label_number(suffix = " crores"))+
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

# sum up all states and calculate a value for india
india_totalburden_inthousands_cvd <- daly_totalburden_cvd_2030 |> 
  drop_na() |> 
  summarise(economic_burden_2030 = sum(totaleconBurden_2030)) |> 
  mutate(state = "India")

india_totalburden_inthousands_cvd |> 
  filter(state=="India")

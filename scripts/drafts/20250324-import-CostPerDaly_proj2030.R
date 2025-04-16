# import cpd-2021.csv from data folder

library(readr)
library(tidyverse)
cpd2021<- read_csv(here::here("data","cpd-2021.csv"))

# merge cpd2021 abd india_paf_economy based on state variable
names(cpd2021)
# cpd2030 is 33% of cpd_2019

growth_rate <- (1 + 0.03)^9  # ≈ 1.305

growth_rate <- (1 + 0.03)^9  # compound growth

cpd2030 <- cpd2021 |> 
  mutate(CpD_2030 = cpd_2021 * growth_rate) |> 
  select(State, CpD_2030)



india_paf_economy_cpd2030 <- paf_data_formapviz_economy_2030 |> 
  left_join(cpd2030, by = "State") |> 
  dplyr::select(State, -Year, paf_daly, CpD_2030) 

#multiply to get the economic burden total
daly_totalburden_cvd_2030 <- india_paf_economy_cpd2030 |> 
  mutate(economic_burden_2030 = paf_daly * CpD_2030) |> 
  dplyr::select(State, economic_burden_2030) |> 
  arrange(desc(economic_burden_2030))


# save as rds and csv
saveRDS(daly_totalburden_cvd_2030, here::here("data","daly_totalburden_cvd_2030.rds"))
write_csv(daly_totalburden_cvd_2030, here::here("data","daly_totalburden_cvd_2030.csv"))  


# bar charts
daly_totalburden_cvd_2030 |> 
  drop_na() |>
  ggplot(aes(x=reorder(State, economic_burden_2030), y=economic_burden_2030/10000000))+
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
  summarise(economic_burden_2030 = sum(economic_burden_2030)) |> 
  mutate(state = "India")

india_totalburden_inthousands_cvd |> 
  filter(state=="India")

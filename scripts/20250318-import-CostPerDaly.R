# import cpd-2019.csv from data folder

library(readr)
library(tidyverse)
cpd2019 <- read_csv(here::here("data","cpd-2019.csv"))

# merge cpd2019 abd india_paf_economy based on state variable
names(cpd2019)
india_paf_economy_cpd2019 <- paf_data_formapviz_economy |> 
  left_join(cpd2019, by = c("state" = "State")) |> 
  dplyr::select(state, -Year, paf_daly, -paf_yld, -paf_yll, CpD_2019) 

#multiply to get the economic burden total
daly_totalburden_inthousands_cvd <- india_paf_economy_cpd2019 |> 
  mutate(economic_burden_2019 = paf_daly * CpD_2019) |> 
  dplyr::select(state, economic_burden_2019) |> 
  arrange(desc(economic_burden_2019))


# save as rds and csv
saveRDS(daly_totalburden_inthousands_cvd, here::here("data","daly_totalburden_inthousands_cvd.rds"))
write_csv(daly_totalburden_inthousands_cvd, here::here("data","daly_totalburden_inthousands_cvd.csv"))  


# bar charts
daly_totalburden_inthousands_cvd |> 
  ggplot(aes(x=reorder(state, economic_burden_2019), y=economic_burden_2019))+
  geom_bar(stat="identity")+
  coord_flip()+
  labs(title="Economic burden of CVD in India in 2019",
       x="State",
       y="Economic burden in thousands")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# sum up all states and calculate a value for india
india_totalburden_inthousands_cvd <- daly_totalburden_inthousands_cvd |> 
  summarise(economic_burden_2019 = sum(economic_burden_2019)) |> 
  mutate(state = "India")

india_totalburden_inthousands_cvd |> 
  filter(state=="India")

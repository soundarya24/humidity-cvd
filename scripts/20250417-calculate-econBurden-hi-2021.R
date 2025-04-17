# import rds file from output folder

data_cost_per_daly_2021 <- readRDS(
  here::here(
    "output",
    "cost_per_daly_2021_relhum.rds"  
  )
)
names(data_cost_per_daly_2021)

source(here::here(
  "scripts",
  "20250417-import-pafabsdaly-hi-2021.R"
))

names(absDALY2021_hi)

# join both by State

library(tidyverse)
total_daly_costcal_2021 <- data_cost_per_daly_2021 |> 
  left_join(
    absDALY2021_hi,
    by = c("State")
  )

names(total_daly_costcal_2021)

total_daly_costcal_2021 <- total_daly_costcal_2021 |> 
  mutate(totalcost_avgheatindexhigh=paf_daly_avghi975*cpd_2021)

# save as rh_daly_totalburden_cvd_2021.rds in output folder using here

saveRDS(
  total_daly_costcal_2021,
  here::here(
    "output",
    "hi_daly_totalburden_cvd_2021.rds"
  )
)

# save as csv

write.csv(
  total_daly_costcal_2021,
  here::here(
    "output",
    "hi_daly_totalburden_cvd_2021.csv"
  ),
  row.names = FALSE
)

# visualize total econ burden in all states by bar charts

total_daly_costcal_2021 |> 
  drop_na(totalcost_avgheatindexhigh) |>
  # sort the bars
  ggplot(aes(x=reorder(State,totalcost_avgheatindexhigh),
             y=totalcost_avgheatindexhigh)) +
  geom_bar(stat="identity") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Total economic burden attributable to high average temperature 2021",
       x="States",
       y="Total cost (INR)") +
  scale_y_continuous(labels = scales::label_number(prefix = "\u20B9", scale = 1)) +  # ₹
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))


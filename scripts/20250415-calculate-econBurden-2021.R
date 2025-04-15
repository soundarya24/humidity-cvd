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
  "20250415-import-pafabsdaly-2021.R"
))

names(absDALY2021)

# join both by State

library(tidyverse)
total_daly_costcal_2021 <- data_cost_per_daly_2021 |> 
  left_join(
    absDALY2021,
    by = c("State")
  )

names(total_daly_costcal_2021)

total_daly_costcal_2021 <- total_daly_costcal_2021 |> 
  mutate(totalcost_relhumhigh=paf_daly_avghum975*cpd_2021)

# visualize total econ burden in all states by bar charts

total_daly_costcal_2021 |> 
  drop_na(totalcost_relhumhigh) |>
  # sort the bars
  ggplot(aes(x=reorder(State,totalcost_relhumhigh), y=totalcost_relhumhigh)) +
  geom_bar(stat="identity") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Total economic burden attributable to high average relative humidity 2021",
       x="States",
       y="Total cost (INR)") +
  scale_y_continuous(labels = scales::label_number(prefix = "\u20B9", scale = 1)) +  # ₹
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

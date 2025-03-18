pacman::p_load("forecast")

combined_data_relhum_forpanel <- readRDS(here::here("data","combined_data_relhum_forpanel_revised.rds"))

head(combined_data_relhum_forpanel)
names(combined_data_relhum_forpanel)
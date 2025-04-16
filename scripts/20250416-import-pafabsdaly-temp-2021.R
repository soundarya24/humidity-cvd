# read dalyabsolute_values_rh.rds file from output folder using ehre


absDALY2021_temp <- readRDS(here::here(
  "output",
  "dalyabsolute_values_temperatures.rds"
))

absDALY2021_temp <- absDALY2021_temp |> 
  dplyr::rename(
    State=state
  )

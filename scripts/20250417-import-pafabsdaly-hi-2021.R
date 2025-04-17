# read dalyabsolute_values_rh.rds file from output folder using ehre


absDALY2021_hi <- readRDS(here::here(
  "output",
  "dalyabsolute_values_HeatIndex.rds"
))

absDALY2021_hi <- absDALY2021_hi |> 
  dplyr::rename(
    State=state
  )

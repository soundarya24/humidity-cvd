# read dalyabsolute_values_rh.rds file from output folder using ehre


absDALY2021 <- readRDS(here::here(
  "output",
  "dalyabsolute_values_rh.rds"
))

absDALY2021 <- absDALY2021 |> 
  dplyr::rename(
    State=state
  )

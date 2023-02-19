library(tidyverse)
library(targets)

tar_read(pt_names) |> dplyr::glimpse()
tar_read(pt_registry) |> dplyr::glimpse()


ids <- (pt_ids)

baseline <- (baselineArrays)
daily <- (dailyArrays)
trd <- (trdArrays)
outcome <- (outArrays)

a <- create_subdata(ids, baseline, daily, trd, outcome, n_days = maxRelevantDays)



daily[["BA001"]]
outcome[["BA001"]]

ba001_trd <- tar_read(weaningsTRD) |>
  dplyr::filter(id_univoco == "BA001")

ba001_trd |>
  dplyr::select(date, ora) |>
  group_by(date) |>
  count()


a <- tar_read(subdata_2_days)

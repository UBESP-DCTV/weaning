library(tidyverse)
library(targets)

tar_read(pt_names) |> dplyr::glimpse()
tar_read(pt_registry) |> dplyr::glimpse()


ids <- tar_read(pt_ids)

baseline <- tar_read(baselineArrays)
daily <- tar_read(dailyArrays)
trd <- tar_read(trdArrays)
outcome <- tar_read(outArrays)

a <- create_subdata(ids, baseline, daily, trd, outcome, n_days = maxRelevantDays)

baseline[1] |>
  str(1)
daily[[1]]



daily[["BA001"]]
outcome[["BA001"]]

ba001_trd <- tar_read(weaningsTRD) |>
  dplyr::filter(id_univoco == "BA001")

ba001_trd |>
  dplyr::select(date, ora) |>
  group_by(date) |>
  count()


a <- tar_read(subdata_2_days)


reg <- tar_read(pt_registry)

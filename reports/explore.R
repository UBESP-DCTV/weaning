library(tidyverse)
library(targets)

tar_read(pt_names) |> dplyr::glimpse()
tar_read(pt_registry) |> dplyr::glimpse()


ids <- tar_read(pt_ids)

baseline <- tar_read(baselineArrays)
daily <- tar_read(dailyArrays)
trd <- tar_read(trdArrays)
outcome <- tar_read(outArrays)


str(baseline, 1)
str(daily[[1]], 1)

a <- create_subdata(ids, baseline, daily, trd, outcome)

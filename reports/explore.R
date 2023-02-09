library(tidyverse)
library(targets)

tar_read(pt_names) |> dplyr::glimpse()
tar_read(pt_registry) |> dplyr::glimpse()


ids <- tar_read(pt_ids)

baseline <- tar_read(baselineArrays)
daily <- tar_read(dailyArrays)
trd <- tar_read(trdArrays)
out <- tar_read(outArrays)

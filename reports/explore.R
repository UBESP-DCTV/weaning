library(tidyverse)
library(targets)

trd <- tar_read(weaningsTRD)
log <- tar_read(weaningsLOG)




log |> filter(is.na(id_pat))

trd |>
  janitor::get_dupes(-file)

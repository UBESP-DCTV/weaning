library(tidyverse)
library(targets)

trd <- tar_read(weaningsTRD)
log <- tar_read(weaningsLOG)


trd
log


trd |>
  janitor::get_dupes(-file)

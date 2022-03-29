library(tidyverse)

weanings <- qs::qread(here::here("data/weaning.qs"))

weanings |>
  glimpse()

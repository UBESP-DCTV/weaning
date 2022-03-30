library(tidyverse)

weanings <- qs::qread(here::here("data/weaning.qs"))

weanings |>
  dplyr::mutate(
    file = str_replace_all(file, "_.*$", "")
  ) |>
  distinct(file)
  glimpse()

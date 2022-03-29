## code to prepare `weanings` dataset goes here
library(furrr)
plan(multisession(workers = availableCores() - 1L))

devtools::load_all()

weanings_trd <- get_data_path() |>
  import_trd_folders(verbose = FALSE)


weanings_trd |>
  qs::qsave(here::here("inst/extdata", "weanings_trd.qs"))

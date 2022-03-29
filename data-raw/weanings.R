## code to prepare `weanings` dataset goes here
library(furrr)
plan(multisession(workers = availableCores() - 1L))

devtools::load_all()

weanings <- get_data_path() |>
  import_trd_folders(verbose = TRUE)

# weanings |>
#   qs::qsave(here::here("inst/extdata", "weanings.qs"))

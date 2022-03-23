library(testthat)
library(checkmate)

library(tidyverse)
library(lubridate)
library(here)


# Functions -------------------------------------------------------

import_trd <- function(path) {
  stopifnot(stringr::str_detect(path, "TRD"))
  assert_file_exists(path)

  # for future development
  headr <- readr::read_lines(path, n_max = 20)

  content <- read_lines(path, skip = 20) |>
    str_subset("Riassunto", negate = TRUE)

  I(content) |>
    readr::read_tsv(col_names = FALSE, col_types = "c") |>
    unheadr::mash_colnames(
      n_name_rows = 2,
      keep_names = FALSE,
      sliding_headers = TRUE
    ) |>
    janitor::remove_empty("cols") |>
    janitor::clean_names() |>
    dplyr::filter(if_any(-.data[["ora"]], ~!is.na(.x))) |>
    dplyr::mutate(
      dplyr::across(dplyr::everything(), readr::parse_guess)
    )
}






# Executions/Experiments ------------------------------------------

here::here("data-raw/AB/AB123_8_TRD.SI") |>
  import_trd() |>
  dplyr::glimpse()







# tests -----------------------------------------------------------

with_reporter(
  # default_reporter(), {
  check_reporter(), {


    context(" ")

    test_that("import_trd works", {
      # setup
      sample_path <- here::here("data-raw/AB/AB123_8_TRD.SI")
      fake_trd_path <- fs::file_temp(pattern = "TRD", ext = "SI")
      fake_path <- fs::file_temp(ext = "SI")

      # evaluation
      res <- import_trd(sample_path)

      # tests
      res |>
        expect_tibble(
          ncols = 21,
          types = c("hms", rep("numeric", 20)),
          min.rows = 1,
          all.missing = FALSE
        )

      import_trd(fake_trd_path) |>
        expect_error("File does not exist")

      import_trd(fake_path) |>
        expect_error("TRD")

    })


  }
)

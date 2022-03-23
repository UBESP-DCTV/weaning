library(testthat)
library(checkmate)

library(tidyverse)
library(lubridate)
library(here)


# Functions -------------------------------------------------------

import_trd <- function(.file_path) {
  stopifnot(stringr::str_detect(.file_path, "TRD"))
  assert_file_exists(.file_path)

  # for future development
  headr <- readr::read_lines(.file_path, n_max = 20)

  content <- read_lines(.file_path, skip = 20) |>
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



import_trd_folder <- function(.dir_path) {
  assert_directory_exists(.dir_path)

  trd_files <- .dir_path |>
    list.files(
      pattern = "TRD",
      full.names = TRUE,
      ignore.case = TRUE
    ) |>
    {\(.x) purrr::set_names(.x, basename(.x))}()

  trd_files |>
    purrr::map_dfr(import_trd, .id = "file")
}



import_trd_folders <- function(.dir_path) {
  assert_directory_exists(.dir_path)

  list.dirs(.dir_path, recursive = FALSE) |>
    {\(.x) purrr::set_names(.x, basename(.x))}() |>
    purrr::map_dfr(import_trd_folder, .id = "folder")
}







# Executions/Experiments ------------------------------------------

here::here("data-raw/AB/AB123_8_TRD.SI") |>
  import_trd() |>
  dplyr::glimpse()


here::here("data-raw/AB") |>
  import_trd_folder()





# tests -----------------------------------------------------------

with_reporter(
  # default_reporter(), {
  check_reporter(), {


    context("TRD")

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
          min.cols = 21,
          types = c("hms", rep("numeric", 20)),
          min.rows = 1,
          all.missing = FALSE
        )



      import_trd(fake_trd_path) |>
        expect_error("File does not exist")

      import_trd(fake_path) |>
        expect_error("TRD")

    })


    test_that("import_trd_folder works", {
      # setup
      sample_folder <- here::here("data-raw/AB")

      # evaluation
      res <- import_trd_folder(sample_folder)

      # tests
      res |>
        expect_tibble(
          min.cols = 24,
          types = c("character", "hms", rep("numeric", 20)),
          min.rows = 6,
          all.missing = FALSE
        )

      expect_equal(names(res)[[1]], "file")
      expect_equal(res[["file"]][[1]], "AB123_8_TRD.SI")


    })

    test_that("import_trd_folders works", {
      # setup
      sample_folder <- here::here("data-raw")

      # evaluation
      res <- import_trd_folders(sample_folder)

      # tests
      res |>
        expect_tibble(
          min.cols = 25,
          types = c(rep("character", 2), "hms", rep("numeric", 20)),
          min.rows = 1e3,
          all.missing = FALSE
        )

      expect_equal(names(res)[[1]], "folder")
      expect_equal(res[["folder"]][[1]], "AB")


    })


  }
)

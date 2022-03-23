library(testthat)
library(checkmate)

library(tidyverse)
library(lubridate)
library(here)

library(furrr)
plan(multisession(workers = availableCores() - 1L))


# Functions -------------------------------------------------------

import_trd <- function(.file_path, verbose = FALSE) {
  stopifnot(stringr::str_detect(.file_path, "TRD"))
  assert_file_exists(.file_path)

  if (verbose) usethis::ui_todo(.file_path)

  # for future development
  headr <- readr::read_lines(.file_path, n_max = 20)

  content <- read_lines(.file_path, skip = 20) |>
    str_subset("Riassunto", negate = TRUE)

  res <- I(content) |>
    readr::read_tsv(
      col_names = FALSE,
      col_types = "c",
      na = c("", "NA", "_")
    ) |>
    unheadr::mash_colnames(
      n_name_rows = 2,
      keep_names = FALSE,
      sliding_headers = TRUE
    ) |>
    janitor::clean_names() |>
    dplyr::filter(if_any(-.data[["ora"]], ~!is.na(.x))) |>
    janitor::remove_empty("cols") |>
    dplyr::mutate(
      dplyr::across(dplyr::everything(), readr::parse_guess)
    )

  if (verbose) usethis::ui_done(.file_path)
  res
}



import_trd_folder <- function(.dir_path, verbose = FALSE) {
  assert_directory_exists(.dir_path)

  if (verbose) usethis::ui_todo(.dir_path)

  trd_files <- normalizePath(.dir_path) |>
    list.files(
      pattern = "TRD",
      full.names = TRUE,
      ignore.case = TRUE
    ) |>
    {
      \(.x) .x |>
        purrr::set_names(
          basename(.x) |> stringr::str_remove("_TRD\\.SI$")
        )
    }()

  res <- trd_files |>
    furrr::future_map_dfr(import_trd, .id = "file", verbose = verbose)

  if (verbose) usethis::ui_done(.dir_path)
  res
}



import_trd_folders <- function(.dir_path, verbose = FALSE) {
  assert_directory_exists(.dir_path)

  normalizePath(.dir_path) |>
  list.dirs(recursive = FALSE, full.names = TRUE) |>
    {\(.x) purrr::set_names(.x, basename(.x))}() |>
    purrr::map_dfr(import_trd_folder, verbose = verbose, .id = "folder")
}







# Executions/Experiments ------------------------------------------

a <- here::here("data-raw/AB/AB123_8_TRD.SI") |>
  import_trd() |>
  dplyr::glimpse()


b <- here::here("data-raw/AB") |>
  import_trd_folder()

c <- here::here("data-raw") |>
  import_trd_folders()


# This is the final computation, it's soo sloooow!
# This workaround avoids to run it unexpectedly
if (FALSE) {
  weaning <- file.path("~/../Desktop/Driving p e Weaning/file_nava") |>
    import_trd_folders(verbose = FALSE)
}



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
      expect_equal(res[["file"]][[1]], "AB123_8")
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


    test_that("problematic characters are managed by import_trd", {
      # setup
      # ISSUE: this has a character in a numeric column
      # SOLUTION: it is an NA; added to the list inside read_tsv(...)
      sample_path <- here::here("data-raw/BG/BG004_1451_TRD.SI")

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

    })

    test_that("import_trd admit empty content file", {
      # setup
      # ISSUE: removing empty columns remove everything
      # SOLUTION: filter before cleaning
      sample_path <- here::here(
        "data-raw/BS/BS012_775_TRD_2014-01-13_15-42-54.SI"
      )

      # evaluation
      res <- import_trd(sample_path)

      # tests
      res |>
        expect_tibble(
          min.cols = 0,
          types = c("hms", rep("numeric", 20)),
          min.rows = 0,
          all.missing = TRUE
        )

    })


  }
)

test_that("import_log works", {

  # setup
  sample_path <- file.path(data_test_path(), "AB/AB123_8_LOG.SI")

  problematic_path <- file.path(
    data_test_path(),
    "VC/VC018_2045_LOG.SI"
  )

  # execution
  res <- suppressMessages(import_log(sample_path))
  types <- purrr::map_chr(res, ~class(.x)[[1]])

  problematic_res <- import_log(problematic_path)
  problematic_types <- problematic_res |>
    purrr::map_chr(~class(.x)[[1]])


  # test
  expected_coltypes <- c(
    id_univoco = "character",
    data = "Date",
    ora = "hms",
    id_info = "integer",
    tipo = "factor",
    informazioni = "character",
    time = "POSIXct"
  )

  expect_tibble(res)
  expect_equal(expected_coltypes, types)

  expect_tibble(problematic_res)
  expect_equal(expected_coltypes, problematic_types)
})




test_that("import_folder works for LOG files", {
  # setup
  sample_folder <- file.path(data_test_path(), "AB")

  # evaluation
  res <- suppressMessages(import_folder(sample_folder, "LOG"))

  # tests
  res |>
    expect_tibble(
      types = c(
        "character",  # Added
        "numeric", "Date", "hms", " integer", "factor", "character",
        "POSIXct", "POSIXt"
      ),
      min.rows = 6
    )

  expect_equal(names(res)[[1]], "file")
  expect_equal(res[["file"]][[1]], "AB123_8")
})


test_that("import_folders works for LOG filed", {
  # setup
  sample_folder <- data_test_path()

  # evaluation
  res <- suppressMessages(import_folders(sample_folder, "LOG"))

  # tests
  res |>
    expect_tibble(
      types = c(
        rep("character", 2),  # Added
        "numeric", "Date", "hms", " integer", "factor", "character",
        "POSIXct", "POSIXt"
      ),
      ncols = 9,
      nrows = 1696
    )

  expect_equal(names(res)[[1]], "folder")
  expect_equal(res[["folder"]][[1]], "AB")
})

test_that("manage problematic foders with files without dates", {
  # setup
  problematic_folder <- file.path(data_test_path(), "VC")

  # eval
  res <- suppressMessages(import_folder(problematic_folder, "LOG"))
  types <- purrr::map_chr(res, ~class(.x)[[1]])

  # test
  expect_tibble(res)

  types |>
    expect_equal(
      c(
        file = "character",
        id_univoco = "character",
        data = "Date",
        ora = "hms",
        id_info = "integer",
        tipo = "factor",
        informazioni = "character",
        time = "POSIXct"
      )
    )

})

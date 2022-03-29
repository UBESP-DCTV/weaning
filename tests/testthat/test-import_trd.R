test_that("import_trd works", {
  # setup
  sample_path <- file.path(data_test_path(), "AB/AB123_8_TRD.SI")
  fake_trd_path <- fs::file_temp(pattern = "TRD", ext = "SI")
  fake_path <- fs::file_temp(ext = "SI")

  # evaluation
  res <- import_trd(sample_path)

  # tests
  res |>
    expect_tibble(
      types = c("numeric", "Date", "hms", rep("numeric", 27)),
      min.rows = 1
    )
  unique(res[["date"]]) |>
    expect_equal(lubridate::ymd("2013/07/09"))
  unique(res[["id_pat"]]) |>
    expect_equal(8217)


  import_trd(fake_trd_path) |>
    expect_error("File does not exist")

  import_trd(fake_path) |>
    expect_error("TRD")

})


test_that("import_trd_folder works", {
  # setup
  sample_folder <- file.path(data_test_path(), "AB")

  # evaluation
  res <- import_trd_folder(sample_folder)

  # tests
  res |>
    expect_tibble(
      min.cols = 24,
      types = c("character", "numeric", "Date", "hms", rep("numeric", 20)),
      min.rows = 6
    )

  expect_equal(names(res)[[1]], "file")
  expect_equal(res[["file"]][[1]], "AB123_8")
})


test_that("import_trd_folders works", {
  # setup
  sample_folder <- data_test_path()

  # evaluation
  res <- import_trd_folders(sample_folder)

  # tests
  res |>
    expect_tibble(
      min.cols = 25,
      types = c(
        rep("character", 2),
        "numeric",
        "Date",
        "hms",
        rep("numeric", 20)
      ),
      min.rows = 1e3
    )

  expect_equal(names(res)[[1]], "folder")
  expect_equal(res[["folder"]][[1]], "AB")
})


test_that("problematic characters are managed by import_trd", {
  # setup
  # ISSUE: this has a character in a numeric column
  # SOLUTION: it is an NA; added to the list inside read_tsv(...)
  sample_path <- file.path(data_test_path(), "BG/BG004_1451_TRD.SI")

  # evaluation
  res <- import_trd(sample_path)

  # tests
  res |>
    expect_tibble(
      min.cols = 21,
      types = c("numeric", "Date", "hms", rep("numeric", 20)),
      min.rows = 1
    )

})

test_that("import_trd admit empty content file", {
  # setup
  # ISSUE: removing empty columns remove everything
  # SOLUTION: filter before cleaning
  sample_path <- file.path(
    data_test_path(), "BS/BS012_775_TRD_2014-01-13_15-42-54.SI"
  )

  # evaluation
  res <- import_trd(sample_path)

  # tests
  res |>
    expect_tibble(
      min.cols = 0,
      types = c("numeric", "Date", "hms", rep("numeric", 20)),
      min.rows = 0
    )

})

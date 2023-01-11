test_that("import_trd works", {
  # setup
  sample_path <- file.path(data_test_path(), "AB/AB123_8_TRD.SI")
  fake_trd_path <- fs::file_temp(pattern = "TRD", ext = "SI")
  fake_path <- fs::file_temp(ext = "SI")

  # evaluation
  res <- import_trd(sample_path) |>
    suppressMessages() |>
    suppressWarnings()

  # tests
  res |>
    expect_tibble(
      types = c("numeric", "Date", "hms", rep("numeric", 27)),
      min.rows = 1
    )
  unique(res[["date"]]) |>
    expect_equal(lubridate::ymd("2013/07/09"))
  unique(res[["id_pat"]]) |>
    expect_equal(123)


  import_trd(fake_trd_path) |>
    expect_error("File does not exist")

  import_trd(fake_path) |>
    expect_error("TRD")

})


test_that("import_folder works for TRD files", {
  # setup
  sample_folder <- file.path(data_test_path(), "AB")

  # evaluation
  res <- import_folder(sample_folder) |>
    suppressMessages() |>
    suppressWarnings()

  # tests
  res |>
    expect_tibble(
      types = c(
        "character", "numeric", "Date", "hms", rep("numeric", 20)
      ),
      min.rows = 6
    )

  expect_equal(names(res)[[1]], "file")
  expect_equal(res[["file"]][[1]], "AB123_8")
})


test_that("import_folder works for TRD files", {
  # setup
  sample_folder <- file.path(data_test_path(), "VC")

  # evaluation
  res <- suppressMessages(import_folder(sample_folder))

  # tests
  res |>
    expect_null()
})


test_that("import_folders works for TRD files", {
  # setup
  sample_folder <- data_test_path()

  # evaluation
  res <- import_folders(sample_folder, verbose = TRUE) |>
    suppressMessages() |>
    suppressWarnings()

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
  res <- suppressWarnings(import_trd(sample_path))

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
  res <- suppressWarnings(import_trd(sample_path))

  # tests
  res |>
    expect_tibble(
      types = c("numeric", "Date", "hms", rep("numeric", 20)),
      min.rows = 0
    )

})


test_that("Controllare casini sui duplicati", {
  skip("Da verificare i duplicati dell'ora sullo stesso file")

  # setup
  weanings_trd <- system.file(
      "extdata/weanings_trd.qs",
      package = "weaning"
    ) |>
    qs::qread()

  # evaluate
  ## problem <- weanings_trd |>
  ##   janitor::get_dupes(file, id_pat, date, ora)
  # expect_true(nrow(problem) == 0)
  #
  # equivalent to (but faster)
  not_problem <- weanings_trd |>
    dplyr::distinct(file, id_pat, date, ora)

  # test
  expect_equal(nrow(weanings_trd), nrow(not_problem))
})

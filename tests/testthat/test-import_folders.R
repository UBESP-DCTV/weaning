test_that("import_folder correctly remove duplicates", {
  # setup
  sample_folder <- file.path(data_test_path(), "AB")

  # evaluation
  res <- import_folder(sample_folder) |>
    suppressMessages() |>
    suppressWarnings()

  # tests
  res |>
    expect_tibble(nrows = 1445)
})


test_that("warns on wrong match id from patient folder and content", {
  # setup
  sample_folder <- data_test_path(wrong = TRUE)

  expect_warning(
    import_folders(sample_folder) |>
      suppressMessages(),
    "inconsistency between filename and id patient reported inside"
  ) |>
    suppressWarnings()



})

test_that("import_folder correctly remove duplicates", {
  # setup
  sample_folder <- file.path(data_test_path(), "AB")

  # evaluation
  res <- suppressMessages(import_folder(sample_folder))

  # tests
  res |>
    expect_tibble(nrows = 1445)
})

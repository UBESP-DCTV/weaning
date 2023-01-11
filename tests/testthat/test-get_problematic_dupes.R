test_that("get_problematic_dupes works", {
  # setup
  sample_folder <- file.path(data_test_path(), "AB")
  db <- import_folder(sample_folder, "TRD") |>
    suppressMessages() |>
    suppressWarnings()

  # eval
  expect_message({
      prob_dupes <- get_problematic_dupes(db) |>
        suppressWarnings()
    },
    "No duplicate combinations found"
  )

  # test
  expect_tibble(prob_dupes)
})


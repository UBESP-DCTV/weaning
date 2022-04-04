test_that("get_problematic_dupes works", {
  # setup
  sample_folder <- file.path(data_test_path(), "AB")
  db <- suppressMessages(import_folder(sample_folder, "TRD"))

  # eval
  expect_message({
      prob_dupes <- get_problematic_dupes(db)
    },
    "No duplicate combinations found"
  )

  # test
  expect_tibble(prob_dupes)
})

test_that("get_problematic_dupes works", {
  # setup
  sample_folder <- file.path(data_test_path(), "AB")
  db <- import_trd_folder(sample_folder)

  # eval
  prob_dupes <- get_problematic_dupes(db)

  # test
  expect_tibble(prob_dupes)
})

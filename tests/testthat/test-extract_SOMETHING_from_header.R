test_that("extract_id_from_filepath works", {
  # setup
  nice_log_path <- file.path(tempdir(), "AN", "AN004_869_LOG.SI") |>
    normalizePath(mustWork = FALSE)
  ugly_log_path <- file.path(
    tempdir(), "AN", "BA003_1678_LOG_2014-10-02_10-46-55.SI"
  ) |>
    normalizePath(mustWork = FALSE)

  nice_trd_path <- file.path(tempdir(), "AN", "BA003_1671_TRD.SI") |>
    normalizePath(mustWork = FALSE)
  ugly_trd_path <- file.path(
    tempdir(), "AN", "BA003_1678_TRD_2014-09-29_13-51-30.SI"
  ) |>
    normalizePath(mustWork = FALSE)

  # eval
  nice_log_res <- extract_id_from_filepath(nice_log_path)
  ugly_log_res <- extract_id_from_filepath(ugly_log_path)

  nice_trd_res <- extract_id_from_filepath(nice_trd_path)
  ugly_trd_res <- extract_id_from_filepath(ugly_trd_path)

  # test
  expect_equal(nice_log_res, 4)
  expect_equal(ugly_log_res, 3)
  expect_equal(nice_trd_res, 3)
  expect_equal(ugly_trd_res, 3)
})

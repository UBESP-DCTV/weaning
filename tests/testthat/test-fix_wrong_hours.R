test_that("fix_wrong_hours works", {
  # setup
  db_wrong <- tibble::tribble(
    ~file, ~date, ~ora, ~pressione_di_fine_esp_cm_h2o, ~caratteristiche_dinamiche_ml_cm_h2o,
    "CM021_1644", "2014-09-21", "07:35", 4.96, NA,
    "CM021_1644", "2014-09-21", "07:35", 5.12, NA,
    "CM021_1644", "2014-09-22", "07:35", 5.12, NA,
    "FE008_164", "2013-08-27", "13:57", NA, 6.36
  ) |>
    dplyr::mutate(
      date = lubridate::ymd(.data[["date"]]),
      ora = readr::parse_time(.data[["ora"]])
    )
  db_correct <- tibble::tribble(
    ~file, ~date, ~ora, ~pressione_di_fine_esp_cm_h2o, ~caratteristiche_dinamiche_ml_cm_h2o,
    "CM021_1644", "2014-09-21", "07:35", 4.96, NA,
    "CM021_1644", "2014-09-21", "21:02", 5.12, NA,
    "CM021_1644", "2014-09-22", "07:35", 5.12, NA,
    "FE008_164", "2013-08-27", "05:25", NA, 6.36
  ) |>
    dplyr::mutate(
      date = lubridate::ymd(.data[["date"]]),
      ora = readr::parse_time(.data[["ora"]])
    )

  # eval
  res <- suppressMessages(fix_wrong_hours(db_wrong))

  # test
  expect_equal(res, db_correct)
  expect_equal(
    suppressMessages(fix_wrong_hours(db_correct)),
    db_correct
  )
})

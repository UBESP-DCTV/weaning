test_that("get_date_format works", {
  # setup
  iso <- "2022-04-13"
  ita <- "13-04-2022"
  wrong <- "13-04-22"

  # eval
  res_iso <- get_date_format(iso)
  res_ita <- get_date_format(ita)

  # test
  expect_equal(res_iso, "iso")
  expect_equal(res_ita, "ita")
  expect_error(get_date_format(wrong), "unknown date forma")

})


test_that("parse_weanings_dates works", {
  # setup
  iso <- "2022-04-13"
  ita <- "13-04-2022"

  # eval
  res_iso <- parse_weanings_dates(iso)
  res_ita <- parse_weanings_dates(ita)

  # test
  expect_equal(res_iso, res_ita)
})


test_that("report_skip works", {
  expect_warning({
    res <- report_skip("a")
    expect_equal(res, "a")
  }, "skipped because it reports") |>
    suppressWarnings()

  expect_warning({
    res <- report_skip(letters)
    expect_equal(res, letters)
  }, "skipped because it reports") |>
    suppressWarnings()

})

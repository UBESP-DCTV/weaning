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


test_that("is_ok_filename_id works", {
  # setup
  opt <- tibble::tribble(
    ~content, ~file, ~ok,
    "XX123", "XX123", TRUE,
    "XX0123", "XX123", TRUE,
    "XX01", "XX001", TRUE,
    "", "XX123", TRUE,
    NA_character_, "XX123", TRUE,
    "123", "XX123", TRUE,
    "124", "XX123", FALSE,
    "XX02", "XX001", FALSE,
    "XX124", "XX123", FALSE,
    "XX0124", "XX123", FALSE,
    "XY124", "XX123", FALSE,
    "XXo24", "XX024", TRUE,
    "XXO24", "XX024", TRUE,
    "ciao", "XX123", TRUE
  )


  # eval
  res <- purrr:::map2_lgl(
    opt[["content"]], opt[["file"]], is_ok_filename_id
  )

  # test
  expect_equal(res, opt[["ok"]])
})

test_that("create_pt_ptnames works", {
  # setup
  reason_levels <- c(
    "Altro (specificare)",
    "ARDS",
    "BPCO Riacutizzata",
    "Complicanze Postoperatorie",
    "Polmonite",
    "Scompenso Cardiaco",
    "Sepsi",
    "Trauma - Politrauma"
  )

  pt_names <- tibble::tribble(
    ~id_univoco, ~type, ~sesso, ~anni_eta, ~bmi, ~ibw, ~saps, ~reason,
    "1", "nava", "M", 82, 18, 20, 30,"Sepsi",
    "2", "psv", "F", 74, 26, 21, 50,"Polmonite",
  ) |>
    dplyr::mutate(
      type = factor(type, levels = c("nava", "psv")),
      sesso = factor(sesso, levels = c("F", "M")),
      reason = factor(reason, levels = reason_levels)
    )

  pt_names_wrong_levels <- pt_names |>
    dplyr::mutate(sesso = factor(sesso, levels = c("M", "F")))

  # evaluate
  res_1 <- create_pt_ptnames(pt_names, "1")
  res_2 <- create_pt_ptnames(pt_names, "2")
  res_3 <- create_pt_ptnames(pt_names_wrong_levels, "1")

  # tests
  expect_array(res_1, "integerish", any.missing = FALSE, d = 1)
  expect_equal(res_1, array(c(1, 2, 82, 18, 20, 30, 7), dim = 7))

  expect_array(res_2, "integerish", any.missing = FALSE, d = 1)
  expect_equal(res_2, array(c(2, 1, 74, 26, 21, 50, 5), dim = 7))

  expect_equal(res_3, res_1)
})


test_that("create_weanings works", {
  # setup
  # sbt :=
  #   - se stubato = `-1`,
  #   - se sbt non provato =  `0`,
  #   - se sbt riuscito =  `1`,
  #   - se sbt fallito =  `2`
  weanings <- tibble::tribble(
    ~id_univoco, ~giorno_studio, ~sofa, ~susp_tot, ~ega_ph, ~ega_pao2, ~ega_paco2, ~sbt, ~esito,
    "1", 1, 6, 4, 12, 1, 2, 0, NA,
    "1", 0, 5, 3, 7, 1, 1, 1, "Successo",
    "2", 0, 2, 1, 12, 4, 3, 2, "Fallito"
  )

  # evaluate
  res_1 <- create_pt_weanings(weanings, "1")
  res_2 <- create_pt_weanings(weanings, "2")

  out_1 <- create_pt_output(weanings, "1")
  out_2 <- create_pt_output(weanings, "2")

  # tests
  expect_array(res_1, "integerish", any.missing = FALSE, d = 2)
  ## sbt should be removed!
  expect_equal(
    res_1,
    array(c(5, 6, 3, 4, 7, 12, 1, 1, 1, 2, 1, 0), dim = c(2, 5))
  )
  expect_array(res_2, "integerish", any.missing = FALSE, d = 2)
  expect_equal(
    res_2,
    array(c(2, 1, 12, 4, 3), dim = c(1, 5))
  )

  expect_array(out_1, "integerish", any.missing = FALSE, d = 2)
  expect_equal(out_1, array(c(1, 0, 1, -99), dim = c(2, 2)))
  expect_array(out_2, "integerish", any.missing = FALSE, d = 2)
  expect_equal(out_2, array(c(2, 0), dim = c(1, 2)))

})


test_that("add_sbt works", {
  # setup
  test_reg <- tibble::tribble(
    ~id_univoco, ~giorno_studio, ~susp_tot, ~estubato, ~data_lettura,
      1, 1, 1, TRUE, 1
  )

  # eval
  res <- add_sbt(test_reg)[["sbt"]]

  # test
  expect_integerish(res, lower = -1, upper = 2, any.missing = FALSE)
  expect_length(res, nrow(test_reg))
})



test_that("create_trd works", {
  trd <- targets::tar_read(weaningsTRD)

  # evaluate
  res_an001 <- create_pt_trd(trd, "AN001")
  res_fe002 <- create_pt_trd(trd, "FE002")

  # tests
  if (FALSE) {
    expect_tibble(res_an001, nrows = 7200, ncols = 32)
    expect_false(any(res_an001[["minute"]] == -99))
    expect_false(any(res_an001[["day"]] == -99))
    expect_set_equal(res_an001[["minute"]], 0:1439)
  }

  expect_array(res_an001, "numeric", any.missing = FALSE, d = 3)
  expect_equal(dim(res_an001), c(1440, 5, 30))

  expect_array(res_fe002, "numeric", any.missing = FALSE, d = 3)
  expect_equal(dim(res_fe002), c(1440, 4, 30))

})



test_that("create_pt_output works", {
  pt_reg <- targets::tar_read(pt_registry)

  # evaluate
  res_an001 <- create_pt_output(pt_reg, "AN001")

  # tests
  expect_array(res_an001, "numeric", any.missing = FALSE, d = 2)
  expect_equal(res_an001[[1, 2]], -99)
  expect_equal(dim(res_an001), c(5, 2))

})


test_that("arrays have consistent dimensions", {
  skip_on_ci()
  skip_on_cran()
  skip_on_covr()

  # setup
  ids <- targets::tar_read(pt_ids)

  baseline <- targets::tar_read(baselineArrays) |>
    purrr::set_names(ids)

  daily <- targets::tar_read(dailyArrays) |>
    purrr::set_names(ids)

  trd <- targets::tar_read(trdArrays) |>
    purrr::set_names(ids)

  outcome <- targets::tar_read(outArrays) |>
    purrr::set_names(ids)

  # eval
  twodays_outcome <- purrr::keep(outcome, ~dim(.x)[[1]] >= 2)
  twodays_daily <- purrr::keep(daily, ~dim(.x)[[1]] >= 2)
  twodays_trd <- purrr::keep(trd, ~dim(.x)[[2]] >= 2)

  # test
  expect_lte(length(twodays_trd), length(twodays_outcome))
  expect_lte(length(twodays_trd), length(twodays_daily))
  expect_equal(length(twodays_outcome), length(twodays_daily))

})

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
    ~id_univoco, ~type, ~sesso, ~anni_eta, ~bmi, ~reason,
    "1", "nava", "M", 82, 18, "Sepsi",
    "2", "psv", "F", 74, 26, "Polmonite",
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
  expect_equal(res_1, array(c(1, 2, 82, 18, 7), dim = 5))

  expect_array(res_2, "integerish", any.missing = FALSE, d = 1)
  expect_equal(res_2, array(c(2, 1, 74, 26, 5), dim = 5))

  expect_equal(res_3, res_1)
})


test_that("create_weanings works", {
  # setup
  # sbt :=
  #   stubato = `-1`,
  #   sbt non provato =  `0`,
  #   sbt riuscito =  `1`,
  #   sbt fallito =  `2`
  weanings <- tibble::tribble(
    ~id_univoco, ~giorno_studio, ~sofa, ~cpis, ~susp_tot, ~sbt, ~esito
    "1", 1, 6, 4, 12, 0, -99,
    "1", 0, 5, 3, 7, 1, 1,
    "2", 0, 2, 1, 12, 2, 0
  )

  # evaluate
  res_1 <- create_pt_weanings(weanings, "1")
  res_2 <- create_pt_weanings(weanings, "2")

  out_1 <- create_pt_output(weanings, "1")
  out_2 <- create_pt_output(weanings, "2")

  # tests
  expect_array(res_1, "integerish", any.missing = FALSE, d = 2)
  ## sbt should be removed!
  expect_equal(res_1, array(c(5, 6, 3, 4, 7, 12), dim = c(2, 3)))
  expect_array(res_2, "integerish", any.missing = FALSE, d = 2)
  expect_equal(res_2, array(c(2, 1, 12), dim = c(1, 3)))

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
  res_AN001 <- create_pt_trd(trd, "AN001")

  # tests
  if (FALSE) {
    expect_tibble(res_AN001, nrows = 7200, ncols = 32)
    expect_false(any(res_AN001[["minute"]] == -99))
    expect_false(any(res_AN001[["day"]] == -99))
    expect_set_equal(res_AN001[["minute"]], 0:1439)
  }

  expect_array(res_AN001, "numeric", any.missing = FALSE, d = 3)
  expect_equal(dim(res_AN001), c(1440, 5, 30))

})

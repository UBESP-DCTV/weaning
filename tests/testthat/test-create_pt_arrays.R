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
  weanings <- tibble::tribble(
    ~id_univoco, ~giorno_studio, ~sofa, ~cpis, ~susp_tot,
    "1", 1, 6, 4, 12,
    "1", 0, 5, 3, 7,
    "2", 0, 2, 1, 12
  )

  # evaluate
  res_1 <- create_pt_weanings(weanings, "1")
  res_2 <- create_pt_weanings(weanings, "2")

  # tests
  expect_array(res_1, "integerish", any.missing = FALSE, d = 2)
  expect_equal(res_1, array(c(5, 6, 3, 4, 7, 12), dim = c(2, 3)))

  expect_array(res_2, "integerish", any.missing = FALSE, d = 2)
  expect_equal(res_2, array(c(2, 1, 12), dim = c(1, 3)))

})

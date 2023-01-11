test_that("import_patient works", {

  # setup
  sample_path <- file.path(data_test_path(), "pt_inclusione_test.xlsx")

  # execution
  res <- import_patients(
    testing_time = TRUE, test_path = sample_path
  ) |>
  suppressMessages() |>
  suppressWarnings()

  types <- purrr::map_chr(res, ~class(.x)[[1]])

  # test
  expected_coltypes <- c(
    id_pt = "numeric",
    id_univoco = "character",
    ospedale = "numeric",
    type = "factor",
    sesso = "factor",
    anni_eta = "numeric",
    kg_peso = "numeric",
    cm_altezza = "numeric",
    bmi = "numeric",
    ibw = "numeric",
    reason = "factor",
    osp_in = "Date",
    icu_in = "Date",
    vm_inizio = "Date",
    vm_fine = "Date",
    icu_out = "Date",
    osp_out = "Date",
    esito = "factor"
  )

  expect_tibble(res)
  expect_equal(types, expected_coltypes)
})

test_that("import_patient works", {

  # setup
  sample_path <- file.path(data_test_path(), "pt_registry_giornalieri_test.xlsx")

  # execution
  res <- suppressMessages(import_registry(testing_time = TRUE,
                                          test_path = sample_path))
  types <- purrr::map_chr(res, ~class(.x)[[1]])

  # test
  expected_coltypes <- c(
    type = "factor",
    id_registry = "numeric",
    filter_deleted = "numeric",
    id_univoco = "character",
    id_medico = "numeric",
    data_lettura = "Date",
    ega_ph = "character",
    ega_pao2 = "character",
    ega_paco2 = "character",
    sofa = "numeric",
    cpis = "numeric",
    estubato = "logical",
    reintubato = "logical",
    morto = "logical",
    susp_aspir = "logical",
    susp_tosse = "logical",
    susp_gcs = "logical",
    susp_fcpas = "logical",
    susp_drugs = "logical",
    susp_pafi = "logical",
    susp_peep = "logical",
    susp_vt = "logical",
    susp_rr = "logical",
    susp_distress = "logical",
    susp_ph = "logical",
    susp_rass = "logical",
    stop_fr = "logical",
    stop_distress = "logical",
    stop_spo2 = "logical",
    stop_pas = "logical",
    stop_fc = "logical",
    stop_rass = "logical",
    fail_agit = "logical",
    fail_coma = "logical",
    fail_muscoli = "logical",
    fail_dispnea = "logical",
    fail_rrvt = "logical",
    fail_pao2 = "logical",
    fail_ph = "logical",
    fail_paco2 = "logical",
    fail_sbp = "logical",
    giorno_studio = "difftime",
    susp_tot = "numeric"
  )

  expect_tibble(res)
  expect_equal(types, expected_coltypes)
})

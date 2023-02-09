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


test_that("extrtact_id_from_header works", {
  # setup
  headr <- c(
    "File trend ventilatore	VTD",
    "Nome del Paziente:	nnnnnnn sssssssss",
    "ID Paziente:	fe001",
    "N° di serie Ventilatore:	 47438",
    "Versione Software del Ventilatore:	Systema:	v6.01	Pannello: 	v22.06.01	Pannello Condiviso: 	v22.06.00	Respirazione: 	v22.06.01	Respiraz. Condivisa: 	v22.06.00	Monitor: 	v22.06.02	Monitor Condiviso: 	v22.06.00	Flusso Esp.: 	v22.06.00	Flusso Esp. Condiviso: 	v22.06.00",
    "Data creazione del file:	14/07/2013",
    "",
    "Controllo Pre-uso:	23/06/2013	12:40:15	Passato	Test circuito paziente:	23/06/2013	12:40:15	Passato",
    "Dettagli controllo Pre-uso:	Test interni: Passato, Test barometro: Passato, Test erogazione gas: Passato, Test perdita interna: Passato, Test trasduttore di pressione: Passato, Test valvola di sicurezza: Passato, Test cella/sensore O₂: Passato, Test trasduttore di flusso: Passato, Test switch batteria: Passato, Test circuito paziente: Passato, Test dello stato di allarme: Passato",
    "Simbolo decimale:	."
  )

  headr_noid <- c(
    "File trend ventilatore	VTD",
    "Nome del Paziente:	nnnnnnn sssssssss",
    "ID Paziente:	",
    "N° di serie Ventilatore:	 47438",
    "Versione Software del Ventilatore:	Systema:	v6.01	Pannello: 	v22.06.01	Pannello Condiviso: 	v22.06.00	Respirazione: 	v22.06.01	Respiraz. Condivisa: 	v22.06.00	Monitor: 	v22.06.02	Monitor Condiviso: 	v22.06.00	Flusso Esp.: 	v22.06.00	Flusso Esp. Condiviso: 	v22.06.00",
    "Data creazione del file:	14/07/2013",
    "",
    "Controllo Pre-uso:	23/06/2013	12:40:15	Passato	Test circuito paziente:	23/06/2013	12:40:15	Passato",
    "Dettagli controllo Pre-uso:	Test interni: Passato, Test barometro: Passato, Test erogazione gas: Passato, Test perdita interna: Passato, Test trasduttore di pressione: Passato, Test valvola di sicurezza: Passato, Test cella/sensore O₂: Passato, Test trasduttore di flusso: Passato, Test switch batteria: Passato, Test circuito paziente: Passato, Test dello stato di allarme: Passato",
    "Simbolo decimale:	."
  )
  # eval
  id <- extract_id_from_header(headr)
  id_noid <- extract_id_from_header(headr_noid)


  # test
  expect_equal(id, "FE001")
  expect_equal(id_noid, NA_character_)
})



test_that("extract_date_from_header works", {
  # setup
  slashed <- "Data creazione del file:	07/07/2013"
  dashed <- "Data creazione del file:	2013-07-07"

  # eval
  slashed_parsed <- extract_date_from_header(slashed)
  dashed_parsed <- extract_date_from_header(dashed)

  # test
  expect_equal(slashed_parsed, lubridate::dmy("07/07/2013"))
  expect_equal(dashed_parsed, lubridate::ymd("2013-07-07"))
})

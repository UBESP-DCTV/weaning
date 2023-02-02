create_pt_trd <- function(db, pt_id) {
  check_array_creation_inputs(db, pt_id)

}

create_pt_log <- function(db, pt_id) {
  check_array_creation_inputs(db, pt_id)

}

create_pt_ptnames <- function(db, pt_id) {
  check_array_creation_inputs(db, pt_id)

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

  db |>
    dplyr::filter(.data[["id_univoco"]] == pt_id) |>
    dplyr::select(
      dplyr::all_of(
        c("type", "sesso", "anni_eta", "bmi", "reason")
      )
    ) |>
    dplyr::mutate(
      type = factor(.data[["type"]], levels = c("nava", "psv")),
      sesso = factor(.data[["sesso"]], levels = c("F", "M")),
      reason = factor(.data[["reason"]], levels = reason_levels)
    ) |>
    unlist(use.names = FALSE) |>
    array()

}

create_pt_weanings <- function(db, pt_id) {
  check_array_creation_inputs(db, pt_id)

  db |>
    dplyr::filter(.data[["id_univoco"]] == pt_id) |>
    dplyr::arrange(.data[["giorno_studio"]]) |>
    dplyr::select(dplyr::all_of(
      c("sofa", "cpis", "susp_tot")
    )) |>
    as.matrix() |>
    unname() |>
    as.array()

}



add_sbt <- function(db_reg) {
  db_reg |>
    dplyr::group_by(.data[["id_univoco"]]) |>
    dplyr::arrange(.data[["data_lettura"]], .by_group = TRUE) |>
    dplyr::mutate(
      sbt = dplyr::case_when(
        # successo sbt (default = TRUE per SBT al primo giorno)
        .data[["susp_tot"]] == 12 &
          .data[["estubato"]] &
          !lag(.data[["estubato"]], default = FALSE) ~ 1L,
        # sbt non provato (ma fattibile)
        .data[["susp_tot"]] < 12 ~ 0L,
        # fallito sbt
        !.data[["estubato"]] ~ 2L,
        # sbt non provato (e non fattibile)
        TRUE ~ -1L
      )
    ) |>
    dplyr::ungroup()
}




check_array_creation_inputs <- function(db, pt_id) {
  checkmate::assert_data_frame(db)
  checkmate::assert_subset("id_univoco", choices = names(db))
  checkmate::assert_string(pt_id)
  checkmate::assert_subset(pt_id, choices = db[["id_univoco"]])
}

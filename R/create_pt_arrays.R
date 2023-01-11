check_array_creation_inputs <- function(db, pt_id) {
  checkmate::assert_data_frame(db)
  checkmate::assert_subset("id_univoco", choices = names(db))
  checkmate::assert_string(pt_id)
  checkmate::assert_subset(pt_id, choices = db[["id_univoco"]])
}

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
      type = factor(type, levels = c("nava", "psv")),
      sesso = factor(sesso, levels = c("F", "M")),
      reason = factor(reason, levels = reason_levels)
    ) |>
    unlist(use.names = FALSE) |>
    array()

}

create_pt_weanings <- function(db, pt_id) {
  check_array_creation_inputs(db, pt_id)

  db |>
    dplyr::filter(.data[["id_univoco"]] == pt_id) |>
    dplyr::arrange(.data[["giorno_studio"]]) |>
    dplyr::select(
      dplyr::all_of(c("sofa", "cpis", "susp_tot"))
    ) |>
    as.matrix() |>
    unname() |>
    as.array()

}

create_pt_outcome <- function(db, pt_id) {
  check_array_creation_inputs(db, pt_id)
  stop("da weanings")
}

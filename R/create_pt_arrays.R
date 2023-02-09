#' trd
#'
#'
#' @param db weaningsTRD dataset
#' @param pt_id a single value from pt_ids identifiying a patient id
#'
#' @return an array of dim
#'   \[pt, minutes, days, var\] = \[none, 1440, length(giorno_studio), \]
#' @export
#'
#' @examples
#' if (FALSE) create_pt_trd(weaningsTRD, pt_ids)[[1]]
create_pt_trd <- function(db, pt_id) {
  check_array_creation_inputs(db, pt_id)

  var_used <- names(db) |>
    setdiff(c(
      "id_univoco", "folder", "id_pat", "file", "stress_index",
      "et_co2_percent"
    ))

  res <- db[db[["id_univoco"]] == pt_id, var_used, drop = FALSE] |>
    dplyr::distinct() |>
    dplyr::arrange(.data[["date"]], .data[["ora"]]) |>
    dplyr::mutate(
      day = as.integer(
        .data[["date"]] - min(.data[["date"]], na.rm = TRUE)
      ),
      minute = get_gross_minutes(.data[["ora"]])
    ) |>
    dplyr::select(-dplyr::all_of(c("date", "ora"))) |>
    dplyr::relocate(
      dplyr::all_of(c("day", "minute")),
      .before = dplyr::everything()
    ) |>
    tidyr::complete(
      day = c(0, seq_len(max(.data[["day"]], na.rm = TRUE))),
      fill = list(minute = 0)
    ) |>
    dplyr::with_groups(
      dplyr::all_of("day"),
      tidyr::complete,
      minute = 0:1439
    ) |>
    dplyr::arrange(.data[["day"]], .data[["minute"]])

  res[is.na(res)] <- -99

  purrr::map(
    c(0, seq_len(max(res[["day"]], na.rm = TRUE))),
    ~ {
      var <- setdiff(names(res), c("day", "minute"))
      res[res[["day"]] == .x, var, drop = FALSE] |>
        as.matrix()
    }
  ) |>
    abind::abind(along = 1.5)
}




create_pt_log <- function(db, pt_id) {
  check_array_creation_inputs(db, pt_id)

  message("non la usiamo")
  invisible(NULL)

}

#' ptnames
#'
#' baseline: array \[pt, var\] = \[none, 2\]
#'
#' @param db pt_names dataset
#' @param pt_id a single value from pt_ids identifiying a patient id
#'
#' @return array \[pt, var\] = \[none, 2\]
#' @export
#'
#' @examples
#' if (FALSE) create_pt_ptnames(pt_names, pt_ids[[1]])
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

#' weanings
#'
#' dialy: array \[pt, days, var\] = \[none, length(giorno_studio), 3\]
#'
#' @param db pt_registry dataset
#' @param pt_id a single value from pt_ids identifiying a patient id
#'
#' @return array \[pt, days, var\] = \[none, length(giorno_studio), 3\]
#' @export
#'
#' @examples
#' if (FALSE) create_pt_weanings(pt_registry, pt_ids[[1]])
create_pt_weanings <- function(db, pt_id) {
  check_array_creation_inputs(db, pt_id)

  db |>
    dplyr::filter(.data[["id_univoco"]] == pt_id) |>
    dplyr::arrange(.data[["giorno_studio"]]) |>
    dplyr::select(dplyr::all_of(c("sofa", "cpis", "susp_tot"))) |>
    as.matrix() |>
    unname() |>
    as.array()

}

#' output
#'
#' dialy: array \[pt, days, out\] = \[none, length(giorno_studio), 1\]
#'
#' @param db pt_registry dataset
#' @param pt_id a single value from pt_ids identifiying a patient id
#'
#' @return array \[pt, days, out\] = \[none, length(giorno_studio), 1\]
#' @export
#'
#' @examples
#' if (FALSE) create_pt_output(pt_registry, pt_ids[[1]])
create_pt_output <- function(db, pt_id) {
  check_array_creation_inputs(db, pt_id)

  db |>
    dplyr::filter(.data[["id_univoco"]] == pt_id) |>
    dplyr::arrange(.data[["giorno_studio"]]) |>
    dplyr::select(dplyr::all_of(c("sbt", "esito"))) |>
    dplyr::mutate(
      esito = as.integer(.data[["esito"]] == "Successo") |>
        tidyr::replace_na(-99L)
    ) |>
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

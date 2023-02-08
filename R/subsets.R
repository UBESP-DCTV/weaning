extract_weaning_subset <- function(pt_reg) {
  pt_reg |>
    dplyr::select(
      dplyr::all_of(c("id_univoco", "data_lettura", "esito", "type"))
    ) |>
    dplyr::filter(
      !is.na(.data[["esito"]]),
      .data[["id_univoco"]] %in% c("TS012", "BS002", "NO004")
    ) |>
    dplyr::group_by(.data[["id_univoco"]]) |>
    dplyr::arrange(.data[["data_lettura"]]) |>
    dplyr::ungroup()
}

extract_weaning_log_subset <- function(weaning_log, weaning_subset) {
  weaning_log |>
    dplyr::filter(
      .data[["id_univoco"]] %in% weaning_subset[["id_univoco"]],
      .data[["date"]] %in% weaning_subset[["data_lettura"]]
    )
}


extract_weaning_log_filtered <- function(weaning_log_subset) {
  weaning_log_subset |>
    dplyr::filter(
      .data[["id_info"]] %in% c(0, 267, 291, 321, 322),
      !stringr::str_detect(.data[["informazioni"]], "Silenziamento"),
      !stringr::str_detect(.data[["informazioni"]], "Pre-silenziamento")
    )
}

extract_weaning_trd_subset <- function(weanings_trd, weaning_subset) {
  weanings_trd |>
    dplyr::filter(
      .data[["id_univoco"]] %in% weaning_subset[["id_univoco"]],
      .data[["date"]] %in% weaning_subset[["data_lettura"]]
    )
}

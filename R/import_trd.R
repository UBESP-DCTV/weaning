#' Import TRD file
#'
#' Read and import in a table the signals reported in a TRD file from
#' weanings study. Among the signals, add a column reporting the
#' patient' ID as `id_pat` and the date of the recording as `date`.
#'
#' @param .file_path (chr) path to the TRD file to import
#' @param verbose (lgl, FALSE) would you like to have additional
#'   messages to be signaled?
#'
#' @return a [tibble][tibble::tibble-package] with the imported signals
#'   (i.e. the tabular content plus the date and the patient id) from
#'    the `.file_path` TRD file.
#' @export
#'
#' @examples
#' \dontrun{
#'   library(weaning)
#'
#'   file.path(get_data_path(), "AB/AN001_356_TRD.SI") |>
#'   import_trd()
#' }
import_trd <- function(.file_path, verbose = FALSE) {
  stopifnot(stringr::str_detect(.file_path, "TRD"))
  checkmate::assert_file_exists(.file_path)

  if (verbose) usethis::ui_todo(.file_path)

  headr <- readr::read_lines(.file_path, n_max = 10)
  content <- readr::read_lines(.file_path, skip = 10) |>
    stringr::str_subset("Riassunto", negate = TRUE) |>
    stringr::str_subset("^$", negate = TRUE)


  res <- I(content) |>
    readr::read_tsv(
      col_names = FALSE,
      col_types = "c",
      na = c("", "NA", "_")
    ) |>
    janitor::remove_empty("cols") |>
    unheadr::mash_colnames(
      n_name_rows = 2,
      keep_names = FALSE
    ) |>
    janitor::clean_names() |>
    dplyr::filter(dplyr::if_any(-.data[["ora"]], ~!is.na(.x))) |>
    dplyr::mutate(
      dplyr::across(-.data[["ora"]], readr::parse_double),
      id_pat = extract_id_from_header(headr),
      date = extract_date_from_header(headr),
      ora = readr::parse_time(.data[["ora"]])
    ) |>
    dplyr::relocate(
      .data[["id_pat"]],
      .data[["date"]],
      .before = .data[["ora"]]
    )


  if (verbose) usethis::ui_done(.file_path)
  res
}

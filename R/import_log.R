#' Import LOG file
#'
#' Read and import in a table the information reported in a LOG file
#' from weanings study. Among the information, add a column reporting
#' the patient' ID as `id_pat`.
#'
#' @param .file_path (chr) path to the LOG file to import
#' @param verbose (lgl, FALSE) would you like to have additional
#'   messages to be signaled?
#'
#' @return a [tibble][tibble::tibble-package] with the imported
#'   information (i.e. the tabular content plus the patient id) from the
#'   `.file_path` LOG file.
#' @export
#'
#' @examples
#' \dontrun{
#'   library(weaning)
#'
#'   file.path(get_data_path(), "AB/AN001_356_LOG.SI") |>
#'   import_log()
#' }
import_log <- function(.file_path, verbose = FALSE) {
  stopifnot(stringr::str_detect(.file_path, "LOG"))
  checkmate::assert_file_exists(.file_path)

  if (verbose) usethis::ui_todo(.file_path)

  headr <- readr::read_lines(.file_path, n_max = 10)

  content <- readr::read_lines(.file_path, skip = 10) |>
    stringr::str_subset("Riassunto", negate = TRUE) |>
    stringr::str_subset("^$", negate = TRUE)

  res <- I(content) |>
    readr::read_tsv(
      col_types = c("ctifc"),
      na = c("", "NA", "_")
    ) |>
    janitor::clean_names() |>
    dplyr::mutate(
      id_pat = extract_id_from_filepath(.file_path),
      data = parse_weanings_dates(.data[["data"]])
    ) |>
    dplyr::rename(id_info = .data[["id"]]) |>
    dplyr::relocate(
      .data[["id_pat"]],
      .before = .data[["data"]]
    )

  if (verbose) usethis::ui_done(.file_path)
  res

}
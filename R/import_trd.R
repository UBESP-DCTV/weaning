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
#'   file.path(get_input_data_path(), "AN/AN001_356_TRD.SI") |>
#'   normalizePath() |>
#'   import_trd()
#' }
import_trd <- function(.file_path, verbose = FALSE) {
  stopifnot(stringr::str_detect(.file_path, "TRD"))
  checkmate::assert_file_exists(.file_path)

  if (verbose) usethis::ui_todo(.file_path)

  headr <- readr::read_lines(.file_path, n_max = 10)
  id_check <- extract_id_from_header(headr)  # not always present

  id_pat <- extract_id_from_filepath(.file_path)
  .dir_path <- basename(dirname(.file_path))
  id_univoco <- create_id_univoco(id_pat, .dir_path)

  check_consistency_idpatfile(id_univoco, id_check)


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
    dplyr::distinct(.data[["ora"]], .keep_all = TRUE) |>
    dplyr::mutate(
      dplyr::across(-dplyr::all_of("ora"), readr::parse_double),
      id_univoco = id_univoco,
      date = extract_date_from_header(headr),
      ora = readr::parse_time(.data[["ora"]])
    )

  ora_x <- which(res[["ora"]] == lubridate::hm("23:59"))

  if (length(ora_x) > 0) {
    res_oggi <- res |>
      dplyr::slice(-seq_len(ora_x))

    res_ieri <- res |>
      dplyr::slice(seq_len(ora_x)) |>
      dplyr::mutate(
        date = date - 1
      )

    res <- dplyr::bind_rows(res_ieri, res_oggi)
  }

  res <- res |>
    dplyr::filter(
      dplyr::if_any(
        -dplyr::all_of(c("ora", "id_univoco", "date")),
        ~!is.na(.x))
    ) |>
    dplyr::relocate(
      dplyr::all_of(c("id_univoco", "date")),
      .before = dplyr::all_of("ora")
    )


  if (verbose) usethis::ui_done(.file_path)
  res
}

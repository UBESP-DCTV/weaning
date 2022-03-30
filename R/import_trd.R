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
#' \dontrun{
#'   library(weaning)
#'
#'   file.path(get_data_path(), "AB/AN001_356_TRD.SI") |>
#'   import_trd()
#' }
#'
import_trd <- function(.file_path, verbose = FALSE) {
  stopifnot(stringr::str_detect(.file_path, "TRD"))
  checkmate::assert_file_exists(.file_path)

  if (verbose) usethis::ui_todo(.file_path)

  headr <- readr::read_lines(.file_path, n_max = 20)
  content <- readr::read_lines(.file_path, skip = 20) |>
    stringr::str_subset("Riassunto", negate = TRUE)


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
    dplyr::relocate(id_pat, date, .before = .data[["ora"]])


  if (verbose) usethis::ui_done(.file_path)
  res
}


extract_date_from_header <- function(headr) {
  headr |>
    stringr::str_subset("^Data creazione") |>
    stringr::str_extract("\\d+/\\d+/\\d+") |>
    lubridate::dmy()
}

extract_id_from_header <- function(headr) {
  headr |>
    stringr::str_subset("^Nome del Paziente:") |>
    stringr::str_extract("\\d+") |>
    as.numeric()
}


#' Import TRDs in a folder
#'
#' Read and import all the infromation in the TRDs fils in a provided
#' folder. It merge all the corresponding tables in a single one. It
#' adds to the merged table a column reporting the folder name imported
#'
#' @note the function import TRD only, i.e., they do not need to be
#' the only ones inside the folder.
#'
#' @param .dir_path (chr) path to the folder containing the TRDs files
#'   to import.
#' @param verbose (lgl, FALSE) would you like to have additional
#'   messages to be signaled?
#'
#' @returna [tibble][tibble::tibble-package] with the imported signals
#'   (i.e. the tabular content plus the date and the patient id) from
#'    all the TRDs files inside the `.dir_path` folder.
#' @export
#'
#' \dontrun{
#'   library(weaning)
#'
#'   file.path(get_data_path(), "AB") |>
#'   import_trd_folder()
#' }
#'
import_trd_folder <- function(.dir_path, verbose = FALSE) {
  checkmate::assert_directory_exists(.dir_path)

  if (!verbose) usethis::ui_todo(.dir_path)

  trd_files <- normalizePath(.dir_path) |>
    list.files(
      pattern = "TRD",
      full.names = TRUE,
      ignore.case = TRUE
    ) |>
    {
      \(.x) .x |>
        purrr::set_names(
          basename(.x) |> stringr::str_remove("_TRD\\.SI$")
        )
    }()

  res <- trd_files |>
    furrr::future_map_dfr(
      import_trd,
      .id = "file",
      verbose = verbose,
      .progress = verbose
      )

  if (!verbose) usethis::ui_done(.dir_path)
  res
}



#' Import folders of TRDs folders
#'
#'
#'
#' @param .dir_path
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
import_trd_folders <- function(.dir_path, verbose = FALSE) {
  checkmate::assert_directory_exists(.dir_path)

  .dir_path |>
    list.dirs(recursive = FALSE, full.names = TRUE) |>
    normalizePath() |>
    {\(.x) purrr::set_names(.x, basename(.x))}() |>
    purrr::map_dfr(import_trd_folder, verbose = verbose, .id = "folder")
}

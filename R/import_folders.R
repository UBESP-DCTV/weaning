#' Import weanings files in a folder
#'
#' Read and import all the information in weanings files in a provided
#' folder. It merges all the corresponding tables in a single one. It
#' adds to the merged table a column reporting the folder name imported
#'
#' @note the function import TRD only, i.e., they do not need to be
#' the only ones inside the folder.
#'
#' @param .dir_path (chr) path to the folder containing the TRDs files
#'   to import.
#' @param what (chr, "TRD") code of the file to import, i.e., one of
#'  "TRD" or "LOG."
#' @param verbose (lgl, FALSE) would you like to have additional
#'   messages to be signaled?
#'
#' @return a [tibble][tibble::tibble-package] with the imported signals
#'   (i.e. the tabular content plus the date and the patient id) from
#'    all the TRDs files inside the `.dir_path` folder.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   library(weaning)
#'
#'   trd_explicit <- file.path(get_data_path(), "AB") |>
#'   import_folder("TRD")
#'
#'   trd_implicit <- file.path(get_data_path(), "AB") |>
#'    import_folder()  # "TRD" is the default
#'
#'   identical(trd_implicit, trd_explicit)
#'
#'   log_explicit_only <- file.path(get_data_path(), "AB") |>
#'   import_folder("LOG")
#'
#'   trd_implicit
#'   log_explicit_only
#' }
#'
import_folder <- function(
    .dir_path,
    what = c("TRD", "LOG"),
    verbose = FALSE
) {
  what <- match.arg(what)

  checkmate::assert_directory_exists(.dir_path)

  if (!verbose) usethis::ui_todo(.dir_path)


  trd_files <- normalizePath(.dir_path) |>
    list.files(
      pattern = what,
      full.names = TRUE,
      ignore.case = TRUE
    ) |>
    {\(.x) .x |>
        purrr::set_names(
          basename(.x) |>
            stringr::str_remove(
              paste0("_", what, "\\.SI$")
            )
        )
    }()


  if (length(trd_files)  == 0L) return(NULL)

  import_fct <- what |>
    switch(
      "TRD" = import_trd,
      "LOG" = import_log
    )

  aux <- trd_files |>
    furrr::future_map_dfr(
      import_fct,
      .id = "file",
      verbose = verbose,
      .progress = verbose
    )

  res <- aux |>
    dplyr::distinct(dplyr::across(-.data[["file"]]), .keep_all = TRUE)

  if (!verbose) usethis::ui_done(.dir_path)
  res
}



#' Import folders of weanings files folders
#'
#' Given a folder collecting weanings files grouped inside distinct
#' folders, it reads and imports all the information in the files
#' and it merges all the corresponding tables in a single one. It adds
#' to the merged table a column reporting the main folder name.
#'
#' @param .dir_path (chr) path to the folder containing the folders
#'   which contain TRDs files
#'   to import.
#' @param what (chr, "TRD") code of the file to import, i.e., one of
#'  "TRD" or "LOG."
#' @param verbose (lgl, FALSE) would you like to have additional
#'   messages to be signaled?
#'
#' @return a [tibble][tibble::tibble-package] with the imported signals
#'   (i.e. the tabular content plus the date and the patient id) from
#'    all the TRDs files inside the `.dir_path` folder.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   library(weaning)
#'
#'   import_folders(get_data_path(), "TRD")
#'   import_folders(get_data_path(), "LOG")
#' }
import_folders <- function(
    .dir_path,
    what = "TRD",
    verbose = FALSE
) {
  checkmate::assert_directory_exists(.dir_path)

  .dir_path |>
    list.dirs(recursive = FALSE, full.names = TRUE) |>
    normalizePath() |>
    {\(.x) purrr::set_names(.x, basename(.x))}() |>
    purrr::map_dfr(
      import_folder,
      what = what,
      verbose = verbose,
      .id = "folder"
    )
}

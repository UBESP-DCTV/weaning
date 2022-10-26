#' Save object to a shared location
#'
#' Save all the objects listed in `obj_list` to the location defined
#' by the environmental variables `PRJ_SHARED_PATH` and
#' `OUTPUT_DATA_FOLDER`, i.e., to the
#' `file.path(Sys.getenv("PRJ_SHARED_PATH"), Sys.getenv("OUTPUT_DATA_FOLDER"))`.
#'
#' @param obj_list (list) `{targets}` object(s) to share.
#' @param use_date_version (lgl, TRUE) do you want to prepend datetime
#'  stamp (format: yyyymmddHHMM) to the file name
#'  (separated by a dash, '-')?
#'
#' @return (chr) path(s) of the saved object(s)
#' @export
#'
share_objects <- function(obj_list, use_date_version = TRUE) {
  checkmate::assert_list(obj_list, names = "named")
  checkmate::qassert(use_date_version, "B1")

  file_name <- paste0(names(obj_list), ".rds")

  if (use_date_version) {
    file_name <- lubridate::now() |>
      stringr::str_replace_all("\\W", "") |>
      stringr::str_trunc(12, ellipsis = "") |>
      paste(file_name, sep = "-")
  }

  obj_paths <- file.path(
    Sys.getenv("PRJ_SHARED_PATH"),
    "/../",
    Sys.getenv("OUTPUT_DATA_FOLDER"),
    file_name
  ) |>
    normalizePath(mustWork = FALSE) |>
    set_names(names(obj_list))

  purrr::walk2(obj_list, obj_paths, readr::write_rds)
  obj_paths
}

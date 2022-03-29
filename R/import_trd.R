import_trd <- function(.file_path, verbose = FALSE) {
  stopifnot(stringr::str_detect(.file_path, "TRD"))
  checkmate::assert_file_exists(.file_path)

  if (verbose) usethis::ui_todo(.file_path)

  # for future development
  headr <- readr::read_lines(.file_path, n_max = 20)
  date <- stringr::str_subset(headr, "^Data creazione") |>
    stringr::str_extract("\\d+/\\d+/\\d+") |>
    lubridate::dmy()
  id_pat <- stringr::str_subset(headr, "^Nome del Paziente:") |>
    stringr::str_extract("\\d+") |>
    as.numeric()

  content <- readr::read_lines(.file_path, skip = 20) |>
    stringr::str_subset("Riassunto", negate = TRUE)

  aux <- I(content) |>
    readr::read_tsv(
      col_names = FALSE,
      col_types = "c",
      na = c("", "NA", "_")
    ) |>
    janitor::remove_empty("cols") |>
    unheadr::mash_colnames(
      n_name_rows = 2,
      keep_names = FALSE,
      sliding_headers = TRUE
    ) |>
    janitor::clean_names() |>
    dplyr::filter(dplyr::if_any(-.data[["ora"]], ~!is.na(.x)))

  res <- aux |>
    dplyr::mutate(
      dplyr::across(-.data[["ora"]], readr::parse_double),
      id_pat = .env[["id_pat"]],
      date = .env[["date"]],
      ora = readr::parse_time(.data[["ora"]])
    ) |>
    dplyr::relocate(id_pat, .before = .data[["ora"]]) |>
    dplyr::relocate(date, .before = .data[["ora"]])


  if (verbose) usethis::ui_done(.file_path)
  res
}



import_trd_folder <- function(.dir_path, verbose = FALSE) {
  checkmate::assert_directory_exists(.dir_path)

  if (verbose) usethis::ui_todo(.dir_path)

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
    furrr::future_map_dfr(import_trd, .id = "file", verbose = verbose)

  if (verbose) usethis::ui_done(.dir_path)
  res
}



import_trd_folders <- function(.dir_path, verbose = FALSE) {
  checkmate::assert_directory_exists(.dir_path)

  normalizePath(.dir_path) |>
    list.dirs(recursive = FALSE, full.names = TRUE) |>
    {\(.x) purrr::set_names(.x, basename(.x))}() |>
    purrr::map_dfr(import_trd_folder, verbose = verbose, .id = "folder")
}


extract_date_from_header <- function(headr) {
  headr |>
    stringr::str_subset("^Data creazione") |>
    stringr::str_extract("\\d+/\\d+/\\d+") |>
    lubridate::dmy()
}

extract_id_from_filepath <- function(filepath) {
  filepath |>
    basename() |>
    stringr::str_extract("(?<=^[A-Z]{2})\\d+") |>
    readr::parse_number()
}

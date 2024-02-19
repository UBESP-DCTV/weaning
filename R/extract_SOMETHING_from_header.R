extract_date_from_header <- function(headr) {
  x <- headr |>
    stringr::str_subset("^Data creazione") |>
    stringr::str_extract("\\d+[/-]\\d+[/-]\\d+")

  x |>
    readr::parse_date(
      lubridate::guess_formats(x, orders = c("Ymd", "dmY"))[[2]]
    )


}

extract_id_from_filepath <- function(filepath) {
  filepath |>
    basename() |>
    stringr::str_extract("(?<=^[A-Z]{2})\\d+") |>
    readr::parse_number()
}

extract_id_from_header <- function(headr) {
  headr |>
    stringr::str_subset("^ID Paziente") |>
    stringr::str_extract("[a-zA-Z]{2}\\d{3}$") |>
    stringr::str_to_upper()
}

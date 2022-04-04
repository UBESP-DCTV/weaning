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

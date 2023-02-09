fix_wrong_hours <- function(db) {
  corrections <- tibble::tribble(
    ~file,        ~date,         ~ora,    ~pressione_di_fine_esp_cm_h2o,
    ~caratteristiche_dinamiche_ml_cm_h2o, ~ora_giusta,
    "CM021_1644",  "2014-09-21", "07:35", 5.12,    NA,  "21:02",
    "FE008_164",   "2013-08-27", "13:57", NA,    6.36,  "05:25",
    "LG014_919",   "2014-02-02", "17:04", NA,   50.60,  "00:12",
    "LG014_956",   "2014-02-07", "14:49", NA,   41.00,  "23:07",
    "TS016_1951",  "2015-01-05", "10:54", NA,   51.50,  "03:34"
  ) |>
    dplyr::mutate(
      date = lubridate::ymd(.data[["date"]]),
      ora = readr::parse_time(.data[["ora"]]),
      ora_giusta = readr::parse_time(.data[["ora_giusta"]])
    )

  for (i in seq_len(nrow(corrections))) {
    correction <- corrections[i, , drop = FALSE]
    are_to_change <- check_row_to_change(db, correction)

    if (sum(are_to_change) == 1) {
      db[[which(are_to_change), "ora"]] <- correction[["ora_giusta"]]
      usethis::ui_done("row for file {correction[['file']]} fixed")
    }
  }
  db
}

check_row_to_change <- function(db, correction) {
  common_vars <- intersect(names(db), names(correction))
  purrr::map(common_vars, ~ db[[.x]] %in% correction[[.x]]) |>
    purrr::reduce(`&`)
}

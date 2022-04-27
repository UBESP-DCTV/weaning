fix_wrong_hours <- function(db) {
  correction <- tibble::tribble(
    ~file, ~date, ~ora, ~pressione_di_fine_esp_cm_h2o, ~caratteristiche_dinamiche_ml_cm_h2o, ~ora_giusta,
    "CM021_1644", "2014-09-21", "07:35", 5.12, NA,  "21:02",
    "FE008_164", "2013-08-27", "13:57", NA,  6.36, "05:25",
    "LG014_919", "2014-02-02", "17:04", NA, 50.60, "00:12",
    "LG014_956", "2014-02-07", "14:49", NA, 41.00, "23:07",
    "TS016_1951", "2015-01-05", "10:54", NA, 51.50, "03:34"
  ) |>
    dplyr::mutate(
      date = lubridate::ymd(.data[["date"]]),
      ora = readr::parse_time(.data[["ora"]]),
      ora_giusta = readr::parse_time(.data[["ora_giusta"]])
    )

  for (i in seq_len(nrow(correction))) {
    to_change <- db[["file"]] == correction[[i, "file"]] &
      db[["date"]] == correction[[i, "date"]] &
      db[["ora"]] == correction[[i, "ora"]] &
      (
        is.na(correction[[i, "pressione_di_fine_esp_cm_h2o"]]) |
          db[["pressione_di_fine_esp_cm_h2o"]] ==
          correction[[i, "pressione_di_fine_esp_cm_h2o"]]
      ) &
      (
        is.na(correction[[i, "caratteristiche_dinamiche_ml_cm_h2o"]]) |
          db[["caratteristiche_dinamiche_ml_cm_h2o"]] ==
          correction[[i, "caratteristiche_dinamiche_ml_cm_h2o"]]
      )

    if (sum(to_change, na.rm = TRUE) == 1) {
      db[[which(to_change), "ora"]] <- correction[[i, "ora_giusta"]]
      usethis::ui_done("row for file {correction[i, 'file']} fixed")
    } else {
      usethis::ui_info(
        "row for file {correction[i, 'file']} doesn't found and skipped."
      )
    }
  }
  db
}

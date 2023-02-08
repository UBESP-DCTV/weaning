get_input_data_path <- function(folder = "") {
  file.path(
    Sys.getenv("PRJ_SHARED_PATH"),
    Sys.getenv("INPUT_DATA_FOLDER"),
    folder
  ) |>
    normalizePath()
}


data_test_path <- function(wrong = FALSE) {

  if (wrong) {
    ifelse(
      dir.exists("../testthat"),
      "../data-test_wrong",
      here::here("tests/data-test_wrong/")
    )
  } else {
    ifelse(
      dir.exists("../testthat"),
      "../data-test",
      here::here("tests/data-test/")
    )
  }
}


view_in_excel <- function(.data) {
  if (interactive()) {
    tmp <- fs::file_temp("excel", ext = "csv")
    readr::write_excel_csv(.data, tmp)
    fs::file_show(tmp)
  }
  invisible(.data)
}


extract_fct_names <- function(path) {
  readr::read_lines(path) |>
    stringr::str_extract_all("^.*(?=`? ?<- ?function)") |>
    unlist() |>
    purrr::compact() |>
    stringr::str_remove_all("[\\s`]+")
}


get_date_format <- function(str_date) {
  is_iso <- stringr::str_detect(str_date, "\\d{4}[-/]\\d{2}[-/]\\d{2}")
  is_ita <- stringr::str_detect(str_date, "\\d{2}[-/]\\d{2}[-/]\\d{4}")

  if (is_iso) return("iso")
  if (is_ita) return("ita")

  usethis::ui_stop(paste0(
    "Date you have passed is {usethis::ui_value(str_date)} ",
    "which is in an unknown date format for us."
  ))
}


parse_weanings_dates <- function(str_date) {
  first_full <- unique(str_date)[[1]]
  date_format <- get_date_format(first_full)

  parser_date <- switch(date_format,
    "iso" = lubridate::ymd,
    "ita" = lubridate::dmy
  )

  parser_date(str_date)
}


get_id <- function(x, id_varname = "id_univoco") {
  checkmate::assert_subset(id_varname, names(x))
  x[[id_varname]]
}


get_gross_minutes <- function(hm) {
  60 * lubridate::hour(hm) + lubridate::minute(hm)
}



create_id_univoco <- function(id_pat, folder) {
    dplyr::case_when(
      id_pat <  10 ~ paste0(folder, "00", id_pat),
      id_pat < 100 ~ paste0(folder, "0", id_pat),
      TRUE ~ paste0(folder, id_pat)
    )
}

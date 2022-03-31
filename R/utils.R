get_data_path <- function() Sys.getenv("WEANING_FOLDER")

data_test_path <- function() {
  ifelse(
    dir.exists("../testthat"),
    "../data-test",
    here::here("tests/data-test/")
  )
}

view_in_excel <- function(.data) {
  if (interactive()) {
    tmp <- fs::file_temp("excel", ext = "csv")
    readr::write_excel_csv(.data, tmp)
    fs::file_show(tmp)
  }
  invisible(.data)
}


`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}


extract_fct_names <- function(path) {
  readr::read_lines(path) |>
    stringr::str_extract_all("^.*(?=`? ?<- ?function)") |>
    unlist() |>
    purrr::compact() |>
    stringr::str_remove_all("[\\s`]+")
}
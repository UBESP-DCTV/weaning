get_data_path <- function() Sys.getenv("WEANING_FOLDER")

data_test_path <- function() {
  ifelse(
    dir.exists("../testthat"),
    "../data-test",
    here::here("tests/data-test/")
  )
}

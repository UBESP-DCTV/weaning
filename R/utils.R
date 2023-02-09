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


check_consistency_idpatfile <- function(id_univoco, id_check) {


  if (!is.na(id_check) && !identical(id_univoco, id_check)) {
    usethis::ui_stop(c(
      "Patient's ID in filename is",
      "{usethis::ui_value(id_univoco)}",
      "while inside the file is",
      "{usethis::ui_value(id_check)}."
    ))
  }
}


report_skip <- function(files2skip) {
  files2skip |>
    purrr::walk(~usethis::ui_warn(c(
      "file {usethis::ui_value(.x)} skipped because it reports",
      "inconsistency between filename and id patient reported inside."
    )))
}

find_mismatch_files <- function(folder_path) {
  # Get the list of all files in the folder
  files <- list.files(
    path = folder_path,
    pattern = "*.SI",
    full.names = TRUE
  ) |>
    normalizePath()

  is_mismatch <- files |>
    purrr::map_lgl(~ {
      # Read the third line of the file
      third_line <- readLines(.x, n = 3)[3]

      # Extract the id from the filename
      filename_id <- strsplit(basename(.x), "_")[[1]][1] |>
        format_string()

      # Extract the id from the third line, if present
      content_id <- strsplit(third_line, ":")[[1]][2] |>
        stringr::str_remove_all("\\s")

      !(is.na(content_id) | content_id == "")  &&
        stringr::str_to_lower(filename_id) !=
        stringr::str_to_lower(content_id) |>
        format_string()
    })

  basename(files[is_mismatch]) |>
    stringr::str_remove("\\.SI$")
}

format_string <- function(input_string) {
  # Extract the letter part and the number part of the input string
  letter_part <- substr(input_string, 1, 2)
  number_part <- as.numeric(substr(input_string, 3, nchar(input_string)))

  # Use sprintf to format the number part with leading zeros to make
  #  it have exactly 3 digits
  formatted_number <- sprintf("%03d", number_part)

  # Concatenate the letter part and the formatted number part to get
  #  the final formatted string
  paste0(letter_part, formatted_number)
  }

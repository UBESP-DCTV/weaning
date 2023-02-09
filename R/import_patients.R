#' Import Patient DB Excel file
#'
#' Read and import in a table the clinical and demographic info
#' reported in an excel file of the weanings study.
#'
#' @param verbose (lgl, FALSE) would you like to have additional
#'   messages to be signaled?
#' @param testing_time (lgl) is test time?
#' @param test_path (chr) test folder
#'
#' @return a [tibble][tibble::tibble-package] with the imported data
#'   (i.e. the tabular content with demographic and clinical info) from
#'    the folder right outside the LOG and TRD folder.
#' @export
#'
#' @examples
#' \dontrun{
#'   library(weaning)
#'
#'   import_patients()
#' }
import_patients <- function(
    verbose = FALSE,
    testing_time = FALSE,
    test_path = "",
    patient_to_remove = character()
) {
  path_input <- file.path(
      get_input_data_path(),
      "/../",
      "pt_names dati inclusione.xlsx"
    ) |>
    normalizePath()

  # Testing time override
  if (testing_time == TRUE) path_input <- test_path

  stopifnot(stringr::str_detect(path_input, "inclusione"))
  checkmate::assert_file_exists(path_input)

  if (verbose) usethis::ui_todo(path_input)



  res <- readxl::read_xlsx(
      path = path_input,
      sheet = "pt_names",
      guess_max = 5
    ) |>
    dplyr::filter(!.data[["id_univoco"]] %in% patient_to_remove) |>
    dplyr::select(
      "id_pt",
      "id_univoco",
      "ospedale",
      "type",
      "sesso",
      "anni_eta",
      "kg_peso",
      "cm_altezza",
      "bmi", "ibw",
      "name reason_mv",
      dplyr::starts_with("data"),
      dplyr::starts_with("esito")
    ) |>
    dplyr::select(-"data_reclutamento") |>
    dplyr::rename(
      reason = .data[["name reason_mv"]],
      osp_in = .data[["data_ricovero ospedaliero"]],
      icu_in = .data[["data_ricovero_terapia intensiva"]],
      vm_inizio = .data[["data_inizio ventilazione meccanica"]],
      vm_fine = .data[["data_fine ventilazione meccanica"]],
      icu_out = .data[["data_out_ti"]],
      osp_out = .data[["data_dimissione"]],
      esito = .data[[paste0(
        "esito_osp ",
        "0 non impostato 1 dimesso 2 deceduto icu 3 deceduto osp"
        )]]
    ) |>
    dplyr::mutate(
      type = forcats::as_factor(.data[["type"]]),
      sesso = forcats::as_factor(.data[["sesso"]]),
      bmi = as.double(.data[["bmi"]]),
      ibw = as.double(.data[["ibw"]]),
      reason = forcats::as_factor(.data[["reason"]]),
      esito = forcats::as_factor(.data[["esito"]]) |>
        forcats::fct_recode(
          "dimesso" = "1",
          "deceduto icu" = "2",
          "deceduto osp" = "3"
        )
    )  |>
    dplyr::mutate(
      osp_in = lubridate::as_date(.data[["osp_in"]]),
      icu_in = lubridate::as_date(.data[["icu_in"]]),
      vm_inizio = lubridate::as_date(.data[["vm_inizio"]]),
      vm_fine = lubridate::as_date(.data[["vm_fine"]]),
      osp_out = lubridate::as_date(.data[["osp_out"]]),
      icu_out = lubridate::as_date(.data[["icu_out"]])
    )

  if (verbose) usethis::ui_done(path_input)
  res
}

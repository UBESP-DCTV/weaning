#' Import Patient daily registry Excel file
#'
#' Read and import in a table daily info about clinical situation of
#' each patient in the weanings study, collected on an excel file.
#'
#' @param verbose (lgl, FALSE) would you like to have additional
#'   messages to be signaled?
#'
#' @return a [tibble][tibble::tibble-package] with the imported data
#'   (i.e. the tabular content with a row for each day for each patient)
#'  from the folder right outside the LOG and TRD folder.
#' @export
#'
#' @examples
#' \dontrun{
#'   library(weaning)
#'
#'   import_registry()
#' }
import_registry <- function(
    verbose = FALSE,
    testing_time = FALSE,
    test_path = "",
    remove_wrong_patients = "NO021"
) {

  path_input <- file.path(
    get_input_data_path(),
    "/../",
    "pt_registry dati giornalieri.xls.xlsx"
  ) |>
    normalizePath()
  stopifnot(stringr::str_detect(path_input, "giornalieri"))
  checkmate::assert_file_exists(path_input)


  # Testing time override
  if (testing_time == TRUE) {
    path_input <- test_path
  }

  if (verbose) usethis::ui_todo(path_input)

  res <- readxl::read_xlsx(
      path = path_input,
      sheet = "pt_registry",
      skip = 2,
      col_types = c(
        "text", "numeric", "numeric",
        "skip", "skip", "skip", "text", "numeric",
        "skip", "skip", "date", "text",
        "text", "text", "numeric",
        "numeric", "skip", "skip", "skip",
        "skip", "skip", "skip", "logical",
        "skip", "logical", "logical", "skip",
        "skip", "skip", "skip", "logical", "logical",
        "logical", "logical", "logical", "logical", "logical",
        "logical", "logical", "logical",
        "logical", #fine suspect criteria
        "logical", "logical", "logical",
        "logical", "logical", "logical", #fine stop criteria
        "logical", "logical", "logical",
        "logical", "logical", "logical", "logical", "skip",
        "logical", "logical", "logical", #fine fail criteria
        "skip", "skip", "skip", "skip",
        "skip", "skip", "skip", "skip", "skip",
        "skip", "skip", "skip", "skip", "skip",
        "skip", "skip", "skip", "skip"
      ),
      col_names = c(
        "type", "id_registry", "filter_deleted", "id_univoco",
        "id_medico", "data_lettura", "ega_ph", "ega_pao2",
        "ega_paco2", "sofa", "cpis",
        "estubato", "reintubato", "morto",
        "susp_aspir", "susp_tosse", "susp_gcs", "susp_fcpas",
        "susp_drugs", "susp_pafi", "susp_peep", "susp_vt",
        "susp_rr", "susp_distress", "susp_ph",
        "susp_rass", "stop_fr", "stop_distress", "stop_spo2",
        "stop_pas", "stop_fc", "stop_rass",
        "fail_agit", "fail_coma", "fail_muscoli",
        "fail_dispnea", "fail_rrvt", "fail_pao2",
        "fail_ph", "fail_paco2", "fail_sbp"
      ),
      na = c("", "NULL")
    ) |>
    dplyr::filter(
      .data[["filter_deleted"]] == 0,
      (!.data[["id_univoco"]] %in% remove_wrong_patients)
    ) |>
    dplyr::mutate(
      type = forcats::as_factor(.data[["type"]]),
      data_lettura = lubridate::as_date(.data[["data_lettura"]]),
      susp_tot = rowSums(dplyr::across(dplyr::starts_with("susp_"))),
      dplyr::across(
        dplyr::starts_with("ega"),
        ~ stringr::str_replace(.x, ",", ".") |> readr::parse_double()
      )
    ) |>
    dplyr::with_groups(
      dplyr::all_of("id_univoco"),
      dplyr::mutate,
      giorno_studio = .data[["data_lettura"]] -
        dplyr::first(.data[["data_lettura"]])
    ) |>
    add_sbt() |>
    dplyr::mutate(
      esito = .data[["sbt"]] |>
        factor(levels = 1:2, labels = c("Successo", "Fallito"))
    )

  if (verbose) usethis::ui_done(path_input)
  res
}

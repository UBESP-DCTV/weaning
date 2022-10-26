#' Import Patient DB Excel file
#'
#' Read and import in a table the clinical and demographic info
#' reported in an excel file of the weanings study.
#'
#' @param verbose (lgl, FALSE) would you like to have additional
#'   messages to be signaled?
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
import_patients <- function(verbose = FALSE) {
  path_input <- file.path( get_input_data_path(),
                           "/../",
                           "pt_names dati inclusione.xlsx") |>
    normalizePath()

  stopifnot(stringr::str_detect(path_input, "inclusione"))
  checkmate::assert_file_exists(path_input)

  if (verbose) usethis::ui_todo(path_input)



  res <- readxl::read_xlsx( path = path_input,
                         sheet = "pt_names",
                         guess_max = 5) |>
    dplyr::select( "id_pt",
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
                   dplyr::starts_with("esito")) |>
    dplyr::select(-"data_reclutamento") |>
    dplyr::rename( reason = `name reason_mv` ,
                   osp_in = `data_ricovero ospedaliero`,
                   icu_in = `data_ricovero_terapia intensiva`,
                   vm_inizio = `data_inizio ventilazione meccanica`,
                   vm_fine = `data_fine ventilazione meccanica`,
                   icu_out = data_out_ti,
                   osp_out = data_dimissione,
                   esito = `esito_osp 0 non impostato 1 dimesso 2 deceduto icu 3 deceduto osp`) |>
    dplyr::mutate( type = forcats::as_factor(type),
                   sesso = forcats::as_factor(sesso),
                   bmi = as.double(bmi),
                   ibw = as.double(ibw),
                   reason = forcats::as_factor(reason),
                   esito = forcats::as_factor(esito))  |>
    dplyr::mutate( osp_in = lubridate::as_date(osp_in),
                 icu_in = lubridate::as_date(icu_in),
                 vm_inizio = lubridate::as_date(vm_inizio),
                 vm_fine = lubridate::as_date(vm_fine),
                 osp_out = lubridate::as_date(osp_out),
                 icu_out = lubridate::as_date(icu_out))

  if (verbose) usethis::ui_done(path_input)
  res
}

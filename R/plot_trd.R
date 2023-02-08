#' Build a plot of most important TRD variables
#'
#' Show on a single plot all relevant variables about a single day
#'  for each patient to spot a weaning attempt:
#'
#' @param weaning_subset (tibble) a tibble containing a list of SBT
#' with a "id_univoco", "data_lettura" and "esito" variable
#' @param color_files (logical) if TRUE, color the lines according to
#' the TRD file from which they originate. If FALSE, lines are colored
#' according to TRD variables
#' @param moving_avg (logical) if TRUE, a 30 minutes moving average
#' window is applied to TRD data
#'
#' @return a [ggplot][ggplot2::ggplot2-package] showing the plot of
#'  mechanical ventilation variables
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   library(weaning)
#'   library(dplyr)
#'
#'   patient_subset <- c("TS012", "BS002", "NO004")
#'
#'   weaning_days <- tar_read(pt_registry) |>
#'     group_by( id_univoco) |>
#'     filter( susp_tot == 12,
#'             estubato == 0)  |>
#'     mutate( esito = factor( x = "Fallito",
#'                             levels = c("Fallito", "Successo")))
#'
#'   weaning_subset <- weaning_days |>
#'     select(id_univoco, data_lettura, esito, type) |>
#'     filter( id_univoco %in% patient_subset) |>
#'     group_by(id_univoco) |>
#'     arrange(data_lettura)
#'
#'   plot_trd( weaning_subset)
#'
#' }
#'
plot_trd <- function(trd,
                     weaning_subset = NA,
                     color_files = FALSE,
                     moving_avg = FALSE) {

  checkmate::assert_logical(color_files)

  if (color_files == FALSE) {
    return(plot_trd_vars(trd, weaning_subset))
  } else if (moving_avg == TRUE) {
    return(plot_trd_mavg(trd, weaning_subset))
  } else {
    return(plot_trd_files(trd, weaning_subset))
  }
}

plot_trd_vars <- function(trd, weaning_subset = NA) {

  checkmate::assert_tibble(weaning_subset,
                           min.rows = 1)

  trd_subset <- trd |>
    dplyr::mutate(id_univoco = ifelse(
      test = id_pat <10,
      yes = paste0(folder, "00", id_pat),
      no = paste0(folder, "0", id_pat) ) ) |>
    dplyr::filter( id_univoco %in% weaning_subset[["id_univoco"]],
                   date %in% weaning_subset[["data_lettura"]]) |>
    dplyr::select(id_univoco,
                  date,
                  ora,
                  lavoro_respiratorio_del_ventilatore_joule_l,
                  lavoro_respiratorio_del_paziente_joule_l,
                  pressione_di_fine_esp_cm_h2o,
                  press_media_vie_aeree_cm_h2o) |>
    # STANDARDIZZARE in [0,1] ?
    # dplyr::mutate(
    #     dplyr::across( lavoro_respiratorio_del_ventilatore_joule_l:press_media_vie_aeree_cm_h2o,
    #            ~as.numeric(scales::rescale(., to = c(0, 1))) )) |>
    tidyr::pivot_longer(cols = 4:7)

  plot <- ggplot2::ggplot(trd_subset,
                          aes( x = ora,
                               y = value,
                               color = name)) +
    ggplot2::geom_step() +
    ggplot2::labs(title = "Weanings TRD",
                  x = "", y = "") +
    ggplot2::facet_wrap(vars(id_univoco, date))

  return(plot)
}

plot_trd_files <- function(trd, weaning_subset = NA) {

  checkmate::assert_tibble(weaning_subset,
                           min.rows = 1)

  trd_subset <- trd |>
    dplyr::mutate( id_univoco = ifelse(
      test = id_pat <10,
      yes = paste0(folder, "00", id_pat),
      no = paste0(folder, "0", id_pat) ) ) |>
    dplyr::filter( id_univoco %in% weaning_subset[["id_univoco"]],
                   date %in% weaning_subset[["data_lettura"]]) |>
    dplyr::select(id_univoco,
                  date,
                  ora,
                  lavoro_respiratorio_del_ventilatore_joule_l,
                  lavoro_respiratorio_del_paziente_joule_l,
                  pressione_di_fine_esp_cm_h2o,
                  press_media_vie_aeree_cm_h2o,
                  file) |>
    # STANDARDIZZARE in [0,1] ?
    # dplyr::mutate(
    #     dplyr::across( lavoro_respiratorio_del_ventilatore_joule_l:press_media_vie_aeree_cm_h2o,
    #            ~as.numeric(scales::rescale(., to = c(0, 1))) )) |>
    tidyr::pivot_longer(cols = 4:7)

  plot <- ggplot2::ggplot(trd_subset,
                          aes(  x = ora,
                                color = file)) +
    ggplot2::geom_line(aes( y = value,
                            group = name)) +
    ggplot2::labs(title = "Weanings TRD",
                  subtitle = "colore per file di origine",
                  x = "", y = "") +
    ggplot2::facet_wrap(vars(id_univoco, date)) +
    ggplot2::theme(legend.position="none")

  return(plot)
}

plot_trd_mavg <- function(trd, weaning_subset = NA) {

  checkmate::assert_tibble(weaning_subset,
                           min.rows = 1)

  trd_subset <- trd |>
    dplyr::mutate( id_univoco = ifelse(
      test = id_pat <10,
      yes = paste0(folder, "00", id_pat),
      no = paste0(folder, "0", id_pat) ) ) |>
    dplyr::filter( id_univoco %in% weaning_subset[["id_univoco"]],
                   date %in% weaning_subset[["data_lettura"]]) |>
    dplyr::select(id_univoco,
                  date,
                  ora,
                  lavoro_respiratorio_del_ventilatore_joule_l,
                  lavoro_respiratorio_del_paziente_joule_l,
                  pressione_di_fine_esp_cm_h2o,
                  press_media_vie_aeree_cm_h2o) |>
    # STANDARDIZZARE in [0,1] ?
    dplyr::mutate(
       dplyr::across( lavoro_respiratorio_del_ventilatore_joule_l:press_media_vie_aeree_cm_h2o,
              ~as.numeric(scales::rescale(., to = c(0, 1))) )) |>
    tidyr::pivot_longer(cols = 4:7)

  plot <- ggplot2::ggplot(trd_subset,
                          aes( x = ora,
                               y = value,
                               color = name)) +
    tidyquant::geom_ma( n = 30,
                        linetype = 1) +
    ggplot2::labs(title = "Moving averages on weanings TRD",
                  x = "", y = "") +
    ggplot2::facet_wrap(vars(id_univoco, date))

  return(plot)
}

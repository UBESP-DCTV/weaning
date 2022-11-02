#' Build a plot of most important TRD variables
#'
#' Show on a single plot all relevant variables about a single day
#'  for each patient to spot a weaning attempt:
#'
#' @param trd_subset (tibble) a tibble originted from a TRD target
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
#'   weaning_subset <- weaning_days %>%
#'     select(id_univoco, data_lettura, esito, type) %>%
#'     filter( id_univoco %in% patient_subset) %>%
#'     group_by(id_univoco) %>%
#'     arrange(data_lettura)
#'
#'   trd_subset <- tar_read(weaningsTRD) %>%
#'     mutate( id_univoco = ifelse(
#'       test = id_pat <10,
#'       yes = paste0(folder, "00", id_pat),
#'       no = paste0(folder, "0", id_pat) ) ) %>%
#'     filter( id_univoco %in% weaning_subset[[ "id_univoco"]],
#'             date %in% weaning_subset[["data_lettura"]])
#'
#'   plot_trd( trd_subset)
#'
#' }
#'
plot_trd <- function(trd_subset = NA) {

  checkmate::assert_tibble(trd_subset,
                           min.rows = 1)

  plot <- ggplot2::ggplot(trd_subset,
                          aes( x = ora)) +
    ggplot2::geom_step(aes(
      y = lavoro_respiratorio_del_ventilatore_joule_l *10) ) +
    ggplot2::geom_step(aes(
      y = lavoro_respiratorio_del_paziente_joule_l *10),
      color = "dark blue") +
    ggplot2::geom_step(aes(
      y = pressione_di_fine_esp_cm_h2o),
              color = "dark green") +
    ggplot2::geom_step(aes(
      y = press_media_vie_aeree_cm_h2o),
              color = "dark orange") +
    ggplot2::labs(title = "Weanings TRD",
         x = "", y = "") +
    ggplot2::facet_wrap(vars(id_univoco, date))

  return(plot)
}

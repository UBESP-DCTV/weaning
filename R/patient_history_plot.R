#' Build a patient history plot
#'
#' Show on a single plot all relevant data about a single patient:
#' - TRD time density plot
#' - LOG time density plot
#' - ICU-stay time span
#' - Mechanical ventilation time span
#' - SBT readiness
#' - if the patient on a certain day was weaned or not
#'
#' @param id_ospedale (chr) 2 letter string of the hospital
#' @param id_paziente (double) id of a patient (between 1 and 99)
#'
#' @return a [ggplot][ggplot2::ggplot2-package] showing the patient
#'  history plot
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   library(weaning)
#'
#'   patient_history_plot( id_ospedale = "TS", id_paziente = 15)
#'
#' }
#'
patient_history_plot <- function(id_ospedale = NA,
                                 id_paziente = NA) {
  # Step 0 - Calculate unique patient id

  checkmate::assert_character(id_ospedale)
  checkmate::assert_int(id_paziente)

  if( id_paziente < 10) {
    id_long <- paste0(id_ospedale, "00", id_paziente)
  } else id_long <- paste0(id_ospedale, "0", id_paziente)


  # Step 1 - get the data from most updated targets

  pat_names <- targets::tar_read(pt_names) %>%
    dplyr::filter(id_univoco == id_long)

  pat_trd <- targets::tar_read(weaningsTRD) |>
    dplyr::filter( folder == id_ospedale,
                   id_pat == id_paziente)

  pat_log <- targets::tar_read(weaningsLOG) %>%
    dplyr::filter( folder == id_ospedale,
                   id_pat == id_paziente)

  pat_registry <- targets::tar_read(pt_registry) %>%
    dplyr::filter(id_univoco == id_long)

  checkmate::assert_tibble(pat_names,
                           min.rows = 1)

  # Step 2 - add to TRD a useful column

  pat_trd <- pat_trd %>%
    dplyr::mutate(
      datetime = lubridate::as_datetime(paste(date,ora))
      )


  # Step 3 - ggplot

  plot <- ggplot2::ggplot() +

    # TRD file
    ggplot2::geom_violin( data = pat_trd,
                 aes( x = datetime,
                      y = "TRD"),
                 scale="count",
                 alpha = 0.2,
                 fill = "blue") +
    ggplot2::geom_segment( data = pat_trd,
                  aes( y = "TRD", yend = "TRD",
                       x = min(datetime),
                       xend = max(datetime)),
                  color = "dark blue",
                  size = 1) +
    ggplot2::geom_point( data = pat_trd,
                aes( y = "TRD",
                     x = min(datetime)),
                color = "dark blue",
                size = 2) +
    ggplot2::geom_point( data = pat_trd,
                aes( y = "TRD",
                     x = max(datetime)),
                color = "dark blue",
                size = 2) +

    # LOG file
    ggplot2::geom_violin( data = pat_log,
                 aes( x = time,
                      y = "LOG"),
                 scale = "count",
                 alpha = 0.2,
                 fill = "violet") +
    ggplot2::geom_segment( data = pat_log,
                  aes( y = "LOG", yend = "LOG",
                       x = min(time),
                       xend = max(time)),
                  color = "purple",
                  size = 1) +
    ggplot2::geom_point( data = pat_log,
                aes( y = "LOG",
                     x = min(time)),
                color = "purple",
                size = 2) +
    ggplot2::geom_point( data = pat_log,
                aes( y = "LOG",
                     x = max(time)),
                color = "purple",
                size = 2) +

    # Registry file
    ggplot2::geom_point( data = pat_registry,
                aes( x = as.POSIXct(data_lettura),
                     y = "Registry",
                     size = susp_tot,
                     color = (susp_tot == 12)),
                show.legend = c(size = FALSE,
                                color = TRUE)) +
    ggplot2::scale_radius(range = c(2,4)) +

    # Patient names file
    ## ICU
    ggplot2::geom_segment( data = pat_names,
                  aes( y = "ICU", yend = "ICU",
                       x = as.POSIXct(icu_in),
                       xend = as.POSIXct(icu_out)),
                  color = "dark grey",
                  size = 2) +
    ggplot2::geom_point( data = pat_names,
                aes( y = "ICU",
                     x = as.POSIXct(icu_in)),
                color = "dark grey",
                size = 4) +
    ggplot2::geom_point( data = pat_names,
                aes( y = "ICU",
                     x = as.POSIXct(icu_out)),
                color = "dark grey",
                size = 4) +
    ggplot2::geom_text( data = pat_names,
               aes( x = as.POSIXct(icu_out),
                    y = "ICU",
                    label = "in ICU"),
               nudge_y = -0.3,
               color = "dark grey") +
    ## MV
    ggplot2::geom_segment( data = pat_names,
                  aes( y = "ICU", yend = "ICU",
                       x = as.POSIXct(vm_inizio),
                       xend = as.POSIXct(vm_fine)),
                  color = "black",
                  size = 1) +
    ggplot2::geom_point( data = pat_names,
                aes( y = "ICU",
                     x = as.POSIXct(vm_inizio)),
                color = "black",
                size = 2) +
    ggplot2::geom_point( data = pat_names,
                aes( y = "ICU",
                     x = as.POSIXct(vm_fine)),
                color = "black",
                size = 2) +
    ggplot2::geom_text( data = pat_names,
               aes( x = as.POSIXct(vm_inizio +2),
                    y = "ICU",
                    label = "in Ventilazione Meccanica"),
               nudge_y = 0.3,
               color = "black") +

    # Estubazioni
    ggplot2::geom_point( data = pat_registry %>%
                           dplyr::filter(estubato == TRUE),
                         aes( y = "Registry",
                              x = as.POSIXct(data_lettura)),
                         color = "black",
                         shape = 4,
                         size = 4) +

    # Title and Labels
    ggplot2::labs( title = "Plot of patient history",
          subtitle = paste("Patient", id_long, "[ X = estubato ]"),
          x = "",
          y = "",
          color = "Ready for SBT") +
    ggplot2::scale_y_discrete(limits = c("LOG",
                                         "TRD",
                                         "Registry",
                                         "ICU"))

  return(plot)
}

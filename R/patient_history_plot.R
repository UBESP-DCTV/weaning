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
#'   patient_history_plot(id_ospedale = "TS", id_paziente = 15)
#'
#' }
#'
patient_history_plot <- function(
    weanings_trd,
    weanings_log,
    pt_names,
    pt_registry,
    id_ospedale = NA,
    id_paziente = NA
) {
  # Step 0 - Calculate unique patient id

  checkmate::assert_character(id_ospedale)
  checkmate::assert_int(id_paziente)

  id_long <- if (id_paziente < 10) {
    paste0(id_ospedale, "00", id_paziente)
  } else {
    paste0(id_ospedale, "0", id_paziente)
  }


  # Step 1 - get the data from most updated targets

  pat_names <- pt_names %>%
    dplyr::filter(.data[["id_univoco"]] == id_long)

  pat_trd <- weanings_trd |>
    dplyr::filter(
      .data[["folder"]] == id_ospedale,
      .data[["id_pat"]] == id_paziente
    )

  pat_log <- weanings_log %>%
    dplyr::filter(
      .data[["folder"]] == id_ospedale,
      .data[["id_pat"]] == id_paziente
    )

  pat_registry <- pt_registry %>%
    dplyr::filter(.data[["id_univoco"]] == id_long)

  checkmate::assert_tibble(pat_names, min.rows = 1)

  # Step 2 - add to TRD a useful column

  pat_trd <- pat_trd %>%
    dplyr::mutate(
      datetime = lubridate::as_datetime(
        paste(.data[["date"]], .data[["ora"]])
      )
    )



  geom_pt_point <- function(
    g, db, type = c("TRD", "LOG"), main_color, second_color
  ) {
    type <- match.arg(type)
    time <- if (type == "TRD") "datetime" else "time"

    g +
    ggplot2::geom_violin(
      data = pat_log,
      aes(x = .data[[time]], y = "LOG"),
      scale = "count",
      alpha = 0.2,
      fill = main_color
    ) +
    ggplot2::geom_segment(
      data = db,
      aes(
        x = min(.data[[time]]),
        y = type, yend = type,
        xend = max(.data[[time]])
      ),
      color = second_color,
      size = 1
    ) +
    ggplot2::geom_point(
      data = db,
      aes(x = min(.data[[time]]), y = type),
      color = second_color,
      size = 2
    ) +
    ggplot2::geom_point(
      data = db,
      aes(x = max(.data[[time]]), y = type),
      color = second_color,
      size = 2
    )
  }

  # Step 3 - ggplot

  plot <- ggplot2::ggplot()

    # TRD file
  plot <- geom_pt_point(plot, pat_trd, "TRD", "blue", "dark blue")

    # LOG file
  plot <- geom_pt_point(plot, pat_log, "LOG", "violet", "purple")

    # Registry file
  plot <- plot +
    ggplot2::geom_point(
      data = pat_registry,
      aes(
        x = as.POSIXct(.data[["data_lettura"]]),
        y = "Registry",
        size = .data[["susp_tot"]],
        color = (.data[["susp_tot"]] == 12)
      ),
      show.legend = c(size = FALSE, color = TRUE)
    ) +
    ggplot2::scale_radius(range = c(2, 4)) +

    # Patient names file
    ## ICU
    ggplot2::geom_segment(
      data = pat_names,
      aes(y = "ICU", yend = "ICU",
           x = as.POSIXct(.data[["icu_in"]]),
           xend = as.POSIXct(.data[["icu_out"]])),
      color = "dark grey",
      size = 2
    ) +
    ggplot2::geom_point(data = pat_names,
                aes(y = "ICU",
                     x = as.POSIXct(.data[["icu_in"]])),
                color = "dark grey",
                size = 4) +
    ggplot2::geom_point(data = pat_names,
                aes(y = "ICU",
                     x = as.POSIXct(.data[["icu_out"]])),
                color = "dark grey",
                size = 4) +
    ggrepel::geom_text_repel(data = pat_names,
               aes(x = as.POSIXct(.data[["icu_out"]]),
                    y = "ICU",
                    label = "in ICU"),
               nudge_y = -0.3,
               color = "dark grey") +
    ## MV
    ggplot2::geom_segment(data = pat_names,
                  aes(y = "ICU", yend = "ICU",
                       x = as.POSIXct(.data[["vm_inizio"]]),
                       xend = as.POSIXct(.data[["vm_fine"]])),
                  color = "black",
                  size = 1) +
    ggplot2::geom_point(data = pat_names,
                aes(y = "ICU",
                     x = as.POSIXct(.data[["vm_inizio"]])),
                color = "black",
                size = 2) +
    ggplot2::geom_point(data = pat_names,
                aes(y = "ICU",
                     x = as.POSIXct(.data[["vm_fine"]])),
                color = "black",
                size = 2) +
    ggrepel::geom_text_repel(data = pat_names,
               aes(x = as.POSIXct(.data[["vm_inizio"]] + 1),
                    y = "ICU",
                    label = "in Ventilazione Meccanica"),
               nudge_y = 0.3,
               color = "black") +

    # Estubazioni
    ggplot2::geom_point(data = pat_registry %>%
                           dplyr::filter(.data[["estubato"]] == TRUE),
                         aes(y = "Registry",
                              x = as.POSIXct(.data[["data_lettura"]])),
                         color = "black",
                         shape = 4,
                         size = 4) +

    # Title and Labels
    ggplot2::labs(title = "Plot of patient history",
          subtitle = paste(
            "Patient", id_long, "[ X = estubato ]"
          ),
          x = "",
          y = "",
          color = "Ready for SBT",
          size = "n. of suspect criteria") +
    ggplot2::scale_y_discrete(limits = c("LOG",
                                         "TRD",
                                         "Registry",
                                         "ICU"))

  return(plot)
}

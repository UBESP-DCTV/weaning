#' Test set check to see if random split produced useful
#' training-validation and test splits
#'
#' @param test_ids
#' @param baseline_input
#' @param daily_input
#' @param trd_input
#'
#' @return a ggplot chart showing different variable distribution
#' for test vs rest of dataset
#' @export
#'
#' @examples
#'  gg_test_check(
#'    test_ids = tar_read(idsTest),
#'    baseline_input = tar_read(pt_names),
#'    daily_input = tar_read(pt_registry),
#'    trd_input = tar_read(weaningsTRD)
#'  )
#'
gg_test_check <- function(
    test_ids,
    baseline_input,
    daily_input,
    trd_input) {

  baseline <- baseline_input |>
    dplyr::mutate( test_set = id_univoco %in% test_ids)
  daily <- daily_input |>
    dplyr::mutate( test_set = id_univoco %in% test_ids) |>
    dplyr::filter(sbt != -1)
  trd <- trd_input |>
    dplyr::mutate( test_set = id_univoco %in% test_ids)

  # continuous variables melt
  baseline_cont <- baseline |>
    dplyr::select(anni_eta, bmi, ibw, saps, test_set) |>
    tidyr::pivot_longer(
      cols = 1:4,
      names_to = "predictor",
      values_to = "value",
      values_transform = as.double
    )

  daily_cont <- daily |>
    dplyr::select(sofa, starts_with("ega_"), susp_tot, test_set) |>
    tidyr::pivot_longer(
      cols = 1:5,
      names_to = "predictor",
      values_to = "value",
      values_transform = as.double
    )

  maxdays_cont <- daily |>
    dplyr::group_by(id_univoco) |>
    dplyr::summarise(
      giorno_finale = max(giorno_studio),
      test_set = unique(test_set)) |>
    dplyr::select(
      giorno_finale,
      test_set) |>
    tidyr::pivot_longer(
      cols = 1,
      names_to = "predictor",
      values_to = "value",
      values_transform = as.integer
    )

  trd_cont <- trd |>
    dplyr::select(
      -file, -id_univoco, -date, -ora,
      -starts_with("el"), -starts_with("et"),
      -starts_with("pressione"),
      -starts_with("resistenza"),
      -starts_with("press_di_pausa"),
      -starts_with("compliance"),
      -starts_with("perdita"),
      -stress_index,
      test_set
    ) |>
    tidyr::pivot_longer(
      cols = 1:20,
      names_to = "predictor",
      values_to = "value",
      values_transform = as.double
    )

  full_cont <- dplyr::bind_rows(
    baseline_cont,
    daily_cont,
    maxdays_cont,
    trd_cont
  )

  # categorical variables melt
  baseline_cat <- baseline |>
    dplyr::select(type, sesso, reason, test_set) |>
    dplyr::mutate(
      reason = reason |>
        forcats::fct_recode(
          "Sepsis" = "Sepsi",
          "Pneumonia" = "Polmonite",
          "Post-surgical complications" = "Complicanze Postoperatorie",
          "Heart Failure" = "Scompenso Cardiaco",
          "COPD exacerbation" = "BPCO Riacutizzata",
          "Other" = "Altro (specificare)",
          "Trauma - Polytrauma" = "Trauma - Politrauma"
          # ARDS doesn't need recoding
        )) |>
    tidyr::pivot_longer(
      cols = 1:3,
      names_to = "predictor",
      values_to = "value",
      values_transform = as.character
    )

  daily_cat <- daily |>
    dplyr::select(sbt, test_set) |>
    dplyr::mutate(
      sbt = sbt |>
        as.character() |>
        forcats::fct_recode(
          # "Already extubated" = "-1",
          "Readiness Testing failure" = "0",
          "SBT success" = "1",
          "SBT failure" = "2"
        )) |>
    tidyr::pivot_longer(
      cols = 1,
      names_to = "predictor",
      values_to = "value",
      values_transform = as.character
    )

  full_cat <- dplyr::bind_rows(
    baseline_cat,
    daily_cat
  )

  #final plot
  ggpubr::ggarrange(
    cont_plot(full_cont),
    cat_plot(full_cat),
    nrow = 1,
    common.legend = TRUE
  )
}

cont_plot <- function(db) {
  db |>
    ggplot2::ggplot(
      ggplot2::aes(value, fill = test_set)) +
    ggplot2::geom_density(alpha = 0.5) +
    ggplot2::facet_wrap(
      ~predictor,
      scales = "free",
      nrow = 6)
}

cat_plot <- function(db) {
  db |>
    ggplot2::ggplot(
      ggplot2::aes(value, fill = test_set)) +
    ggplot2::geom_bar(alpha = 0.5,
             position = "dodge") +
    ggplot2::facet_wrap(~predictor,
               scales = "free",
               ncol = 2) +
    ggplot2::coord_flip()
}

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
    dplyr::select(anni_eta, bmi, ibw, saps, test_set)|>
    dplyr::rename(
      "age" = "anni_eta"
    ) |>
    tidyr::pivot_longer(
      cols = 1:4,
      names_to = "predictor",
      values_to = "value",
      values_transform = as.double
    )

  daily_cont <- daily |>
    dplyr::select(sofa, starts_with("ega_"), susp_tot, test_set)|>
    dplyr::rename(
      "Readiness Test" = "susp_tot"
    ) |>
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
    dplyr::rename(
      "max days" = "giorno_finale"
    ) |>
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
      pressione_di_fine_esp_cm_h2o,
      test_set
    ) |>
    dplyr::rename(
      "dynamic characteristics" = "caratteristiche_dinamiche_ml_cm_h2o",
      "end-expiratory flow" = "flusso_di_fine_esp_l_min",
      "positive end-expiratory flow" = "pressione_di_fine_esp_cm_h2o",
      "minute expired volume" = "vol_minuto_espirato_l_min",
      "current expired volume" = "vol_corrente_espirato_ml",
      "O2 saturation %" = "concentraz_o2_percent",
      "minute inspired volume" = "vol_minuto_inspirato_l_min",
      "current inspired volume" = "vol_corrente_inspirato_ml",
      "mean airway pressure" = "press_media_vie_aeree_cm_h2o",
      "measured respiratory rate" = "frequenza_respiraz_misurata_resp_min",
      "sponteneous respiratory rate" = "freq_spontanea_resp_min",
      "Edi peak" = "picco_edi_m_v",
      "Edi min" = "edi_min_m_v",
      "plateau pressure" = "press_di_picco_delle_vie_aeree_cm_h2o",
      "backup switches" = "passa_a_backup_min",
      "backup percent" = "backup_percent_min",
      "P 0.1" = "p_0_1_cm_h2o",
      "mechanical ventilator respiratory work" = "lavoro_respiratorio_del_ventilatore_joule_l",
      "patient ventilator respiratory work" = "lavoro_respiratorio_del_paziente_joule_l",
      "spontaneous breathing index" = "sbi",
      "spontaneous minute expired volume" = "volume_minuto_espirato_spontaneo_l_min"
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

  # categorical variables summary
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
        ),
      test_set = factor(test_set, labels = c("Training/validation set", "Test set"))
      ) |>
    tbl_summary(
      by = test_set,
      label = list( type ~ "Ventilation mode",
                    sesso ~ "Gender",
                    reason ~ "Reason for MV")
    ) |> add_p()

  daily_cat <- daily |>
    dplyr::select(sbt, test_set) |>
    dplyr::mutate(
      test_set = factor(test_set, labels = c("Training/validation set", "Test set")),
      sbt = sbt |>
        as.character() |>
        forcats::fct_recode(
          # "Already extubated" = "-1",
          "Readiness Testing failure" = "0",
          "SBT success" = "1",
          "SBT failure" = "2"
        ))  |>
    tbl_summary(
      by = test_set,
      label = list(
        sbt ~ "SBT oucome"
      )
    ) |> add_p()

  full_cat <- gtsummary::tbl_stack(tbls = list(baseline_cat, daily_cat))

  #final plot
  list(full_cont, full_cat)
}

cont_plot <- function(db) {
  db |>
    ggplot2::ggplot(
      ggplot2::aes(value, fill = test_set)) +
    ggplot2::geom_density(alpha = 0.33) +
    ggplot2::facet_wrap(
      ~predictor,
      scales = "free",
      nrow = 6)
}


  test_ids <- tar_read(idsTest)
  baseline_input <- tar_read(pt_names)
  daily_input <- tar_read(pt_registry)
  trd_input <- tar_read(weaningsTRD)
  db <- full_cat

build_dailydata <- function(baseline, daily) {

  daily |>
    dplyr::select(id_univoco,
           dplyr::starts_with("ega"),
           sofa,
           susp_tot,
           giorno_studio,
           sbt,
           test_set) |>
    dplyr::group_by(id_univoco) |>
    dplyr::mutate(
      ega_ph = dplyr::lag(ega_ph, default = NA),
      ega_pao2 = dplyr::lag(ega_pao2, default = NA),
      ega_paco2 = dplyr::lag(ega_paco2, default = NA),
      sofa = dplyr::lag(sofa, default = NA),
      susp_tot = dplyr::lag(susp_tot, default = NA)
    ) |>
    dplyr::ungroup()  |>
    filter(sbt != -1) |>
    dplyr::left_join(
      y = baseline |>
        dplyr::select(
          id_univoco,
          type,
          sesso,
          anni_eta,
          bmi,
          ibw,
          saps,
          reason
        ),
      by = "id_univoco"
    )
}

build_xgb_model <- function(dailydata) {
  familiar::summon_familiar(
    data = dplyr::filter(dailydata, test_set == FALSE),
    experiment_dir = file.path(here::here(), "train_xgboost"),
    experimental_design = "cv(fs+mb,10,1)",
    sample_id_column = "id_univoco",
    outcome_type = "multinomial",
    outcome_column = "sbt",
    fs_method = "elastic_net",
    learner = "xgboost_tree",
    evaluation_metric = "balanced_accuracy",
    imputation_method = "simple"
  )
}

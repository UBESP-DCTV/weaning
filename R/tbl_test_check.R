tbl_test_check <- function(
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

  table_baseline <- baseline |>
    dplyr::select(-id_univoco,
                  -dplyr::starts_with("vm"),
                  -dplyr::starts_with("icu"),
                  -dplyr::starts_with("osp")) |>
    dplyr::mutate(reason = reason |>
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
    gtsummary::tbl_summary(
      by = test_set,
      label = list( type ~ "Ventilation mode",
                    sesso ~ "Gender",
                    anni_eta ~ "Age (years)",
                    reason ~ "Reason for MV")
    ) |>
    gtsummary::add_p() |>
    gtsummary::add_q()

  table_daily <- daily |>
    dplyr::select(-id_registry,
           -filter_deleted,
           -id_univoco,
           -id_medico,
           -dplyr::starts_with("susp_"),
           -dplyr::starts_with("stop_"),
           -dplyr::starts_with("fail_"),
           -cpis,
           -data_lettura,
           -giorno_studio,
           susp_tot) |>
    dplyr::mutate(sbt = sbt |>
             as.character() |>
             forcats::fct_recode(
               # "Already extubated" = "-1",
               "Readiness Testing failure" = "0",
               "SBT success" = "1",
               "SBT failure" = "2"
             )
    ) |>
    gtsummary::tbl_summary(
      by= test_set
    ) |>
    gtsummary::add_p() |>
    gtsummary::add_q()

  table_days <- daily |>
    dplyr::group_by(id_univoco) |>
    dplyr::summarise(
      giorno_finale = max(giorno_studio),
      test_set = unique(test_set)) |>
    dplyr::select(
      giorno_finale,
      test_set) |>
    gtsummary::tbl_summary(
      by = test_set,
      label = giorno_finale ~ "Day of study",
      statistic = giorno_finale ~ "{median}  (range: {min}, {max})"
    ) |>
    gtsummary::add_p() |>
    gtsummary::add_q()

  table_trd <- trd_table_create(trd)

  gtsummary::tbl_stack(
    list(
      table_baseline,
      table_daily,
      table_days,
      table_trd
    ),
    group_header = c(
      "Baseline data",
      "Daily registry",
      "Days of MV",
      "MV Track"
    )
  )
}


trd_table_create <- function(trd) {
  trd |>
    dplyr::select(-file,
           -id_univoco,
           -date,
           -ora) |>
    gtsummary::tbl_summary(
      by = test_set
    ) |>
    gtsummary::add_p() |>
    gtsummary::add_q()
}

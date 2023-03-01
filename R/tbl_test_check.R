tbl_test_check <- function(
    test_ids,
    baseline_input,
    daily_input,
    trd_input) {

  baseline <- baseline_input |>
    mutate( test_set = id_univoco %in% test_ids)
  daily <- daily_input |>
    mutate( test_set = id_univoco %in% test_ids) |>
    filter(sbt != -1)
  trd <- trd_input |>
    mutate( test_set = id_univoco %in% test_ids)

  table_baseline <- baseline |>
    select(-id_univoco,
           -starts_with("vm"),
           -starts_with("icu"),
           -starts_with("osp")) |>
    mutate(reason = reason |>
             fct_recode(
               "Sepsis" = "Sepsi",
               "Pneumonia" = "Polmonite",
               "Post-surgical complications" = "Complicanze Postoperatorie",
               "Heart Failure" = "Scompenso Cardiaco",
               "COPD exacerbation" = "BPCO Riacutizzata",
               "Other" = "Altro (specificare)",
               "Trauma - Polytrauma" = "Trauma - Politrauma"
               # ARDS doesn't need recoding
             )) |>
    tbl_summary(
      by = test_set,
      label = list( type ~ "Ventilation mode",
                    sesso ~ "Gender",
                    anni_eta ~ "Age (years)",
                    reason ~ "Reason for MV")
    ) |>
    add_p() |>
    add_q()

  table_daily <- daily |>
    select(-id_registry,
           -filter_deleted,
           -id_univoco,
           -id_medico,
           -starts_with("susp_"),
           -starts_with("stop_"),
           -starts_with("fail_"),
           -cpis,
           -data_lettura,
           -giorno_studio,
           susp_tot) |>
    mutate(sbt = sbt |>
             as.character() |>
             fct_recode(
               # "Already extubated" = "-1",
               "Readiness Testing failure" = "0",
               "SBT success" = "1",
               "SBT failure" = "2"
             )
    ) |>
    tbl_summary(
      by= test_set
    ) |>
    add_p() |>
    add_q()

  table_days <- daily |>
    group_by(id_univoco) |>
    summarise(giorno_finale = max(giorno_studio),
              test_set = unique(test_set)) |>
    select(giorno_finale,
           test_set) |>
    tbl_summary(
      by = test_set,
      label = giorno_finale ~ "Day of study",
      statistic = all_continuous() ~ "{median}  (range: {min}, {max})"
      # digits = ~ 2
    ) |>
    add_p() |>
    add_q()

  table_trd <- trd_table_create(trd)

  tbl_stack(
    list(
      table_baseline,
      table_daily,
      table_days
    ),
    group_header = c(
      "Baseline data",
      "Daily registry",
      "Days of MV"
    )
  )
}

trd_table_create(trd) {
  trd |>
    select(-file,
           -id_univoco,
           -date,
           -ora) |>
    tbl_summary(
      by = test_set
    ) |>
    add_p() |>
    add_q()
}

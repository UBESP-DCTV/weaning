#predicted <- pred_obs["pred0"]
#observed <- pred_obs["observed"]

metric_table <- function(df) {
  #Step 0 : define metrics
  multi_metric <- yardstick::metric_set(
    accuracy,
    bal_accuracy,
    precision,
    mcc,
    recall
  )

  #Step 1: metrics
    df <- df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ factor(.x, levels = c(0,1,2))
  ))

  metrics <- df |>
    multi_metric(
      truth = observed,
      estimate = predicted,
      estimator = "macro_weighted"
  )

  #Step 2 : table
  table <- table(
    df[["predicted"]],
    df[["observed"]]
  )

  return(list(table,metrics))
}


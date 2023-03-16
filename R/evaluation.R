cost_matrix <- matrix(
  # Thesis classes (0skip / 1fail / 2pass)
  #   Truth:        0     1     2
  # Predicted: 0    0    +1    -1
  #            1   NA    +1    -1
  #            2   NA     0     0
  #
  # Code classes (0skip / 1pass / 2fail)
  #   Truth:        0     2     1
  # Predicted: 0    0    +1    -1
  #            2   NA    +1    -1
  #            1   NA     0     0
  #
  c(  0, (35-65)/100, 0,
     -1,  0, -1,
      1,  0,  1),
  nrow = 3, ncol = 3,
  dimnames = list(
    c("0", "1", "2"),
    c("0", "1", "2"))
)


# metrics
clap <- function(data, truth, estimate, na_rm = TRUE, ...) {

  estimate <- data |> select({{estimate}}) |> simplify()
  truth <- data |> select({{truth}}) |> simplify()

  table <- table(estimate, truth)

  score <- sum(
    table * cost_matrix,
    na.rm = na_rm
  ) / sum(table * !is.na(cost_matrix))

  tibble::tribble(
    ~.metric,      ~.estimator,    ~.estimate,
    "CLAP Score",  "element-wise", score
  )
}

clap_bin <- function(data, truth, estimate, na_rm = TRUE, ...) {
  #
  # estimate <- data |> select({{estimate}}) |> simplify()
  # truth <- data |> select({{truth}}) |> simplify()
  #
  # table <- table(estimate, truth)
  #
  # score <- sum(
  #   table * cost_matrix,
  #   na.rm = na_rm
  # ) / sum(table * !is.na(cost_matrix))

  tibble::tribble(
    ~.metric,      ~.estimator,    ~.estimate,
    "CLAP Score",  "element-wise", NA
  )
}

clap <- yardstick::new_class_metric(clap, "maximize")
clap_bin <- yardstick::new_class_metric(clap_bin, "maximize")


multi_metric <- yardstick::metric_set(
  #accuracy,
  yardstick::accuracy,
  yardstick::mcc,
  clap,
  yardstick::precision,
  yardstick::recall
)

multi_metric_bin <- yardstick::metric_set(
  #accuracy,
  yardstick::accuracy,
  yardstick::mcc,
  clap_bin,
  yardstick::precision,
  yardstick::recall
)

bs <- function(model, data, indices) {
  multi_metric(
    data = data[indices,],
    truth = value,
    estimate = {{model}},
    estimator = "macro_weighted"
  ) |>
  dplyr::pull(.estimate)
}

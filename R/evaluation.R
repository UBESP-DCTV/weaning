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
  c(  0, NA, NA,
     -1,  0, -1,
      1,  0,  1),
  nrow = 3, ncol = 3,
  dimnames = list(
    c("0", "1", "2"),
    c("0", "1", "2"))
)

# metrics
clap <- function(data, truth, estimate, na_rm = TRUE, ...) {
  require(rlang)
  estimate <- data |> select({{estimate}}) |> simplify()
  truth <- data |> select({{truth}}) |> simplify()

  score <- sum(
    table(estimate, truth) * cost_matrix,
    na.rm = na_rm
  ) / length(estimate)

  return( tribble(
    ~.metric,      ~.estimator,    ~.estimate,
    "CLAP Score",  "element-wise", score
  ))
}

clap <- yardstick::new_class_metric(clap, "maximize")

require(yardstick)
multi_metric <- yardstick::metric_set(
  #accuracy,
  bal_accuracy,
  mcc,
  #clap,
  precision,
  recall,
  npv
)

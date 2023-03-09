
mv_cost <- tibble::tribble(
  # Thesis classes (0skip / 1fail / 2pass)
  #   Truth:        0     1     2
  # Predicted: 0   +1    +1    -1
  #            1   NA    +1    -1
  #            2   NA    -1    +1
  #
  # Code classes (0skip / 1pass / 2fail)
  #   Truth:        0     2     1
  # Predicted: 0   +1    +1    -1
  #            2   NA    +1    -1
  #            1   NA    -1    +1
  #
  ~truth,   ~estimate, ~cost,
  "0", "0",  +1,
  "0", "2",  NA,
  "0", "1",  NA,
  "2", "0",  +1,
  "2", "2",  +1,
  "2", "1",  -1,
  "1", "0",  -1,
  "1", "2",  -1,
  "1", "1",  +1
)

# metrics
clap <- function(data, truth, estimate, na_rm = TRUE, ...) {

  yardstick::classification_cost(
    data = data,
    truth = !! rlang::enquo(truth),
    estimate = !! rlang::enquo(estimate),
    costs = mv_cost,
    na_rm = na_rm,
    case_weights = NULL
  )
}
clap <- yardstick::new_class_metric(clap, "maximize")

multi_metric <- yardstick::metric_set(
  #accuracy,
  bal_accuracy,
  #mcc,
  #clap,
  precision,
  recall
)

define_keras_model <- function() {

# inputs ----------------------------------------------------------

  input_baseline <- keras::layer_input(
    name = "input_baseline",
    shape = c(5)
  )



# network ---------------------------------------------------------

  baseline_l1 <- input_baseline %>%
    keras::layer_dense(
      name = "baseline_l1", units = 8, activation = "relu"
    )



# Output ----------------------------------------------------------

  out <- baseline_l1 %>%
    keras::layer_dense(name = "out", units = 1, activation = "sigmoid")



# Model -----------------------------------------------------------

  keras::keras_model(
    inputs = input_baseline,
    outputs = out
  )
}

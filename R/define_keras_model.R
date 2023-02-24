define_keras_model <- function() {

# inputs ----------------------------------------------------------

  input_baseline <- keras::layer_input(
    name = "input_baseline",
    shape = c(7)
  )

  input_daily <- keras::layer_input(
    name = "input_daily",
    shape = c(NA, 5)
  )

  input_trd <- keras::layer_input(
    name = "input_trd",
    shape = c(1440, NA, 30)
  )


# network ---------------------------------------------------------


  trd_l1 <- input_trd %>%
    keras::layer_conv_lstm_1d(
      filters = 8,
      kernel_size = 1,
      name = "trd_l1",
      activation = "relu"
    )


  merged_daily_trd <- keras::k_concatenate(c(input_daily, trd_l1)) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activity_regularization(l1 = 1e-1, l2 = 1e-1)

  merged_l3 <- merged_daily_trd %>%
    keras::layer_gru(
      units = 8,
      name = "merged_l3",
      activation = "relu"
    )

  merged_l4 <- keras::k_concatenate(c(merged_l3, input_baseline)) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activity_regularization(l1 = 1e-1, l2 = 1e-1)

  dense_l6 <- merged_l4 %>%
    keras::layer_dense(
      name = "dense_l5",
      units = 8,
      activation = "relu"
    ) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activity_regularization(l1 = 1e-1, l2 = 1e-1) %>%
    keras::layer_dense(
      name = "dense_l6",
      units = 8,
      activation = "relu"
    ) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activity_regularization(l1 = 1e-1, l2 = 1e-1)

# Output ----------------------------------------------------------

  out <- dense_l6 %>%
    keras::layer_dense(name = "out", units = 3, activation = "sigmoid")



# Model -----------------------------------------------------------

  keras::keras_model(
    inputs = c(input_baseline, input_daily, input_trd),
    outputs = out
  )
}

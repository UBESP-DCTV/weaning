define_keras_model <- function(rec_units = 32, dense_unit = 16) {


# custom layers ---------------------------------------------------



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
    shape = c(1440, NA, 21)
  )


# network ---------------------------------------------------------


  trd_l1 <- input_trd |>
    keras::bidirectional(keras::layer_conv_lstm_1d(
      filters = rec_units,
      kernel_size = 5,
      padding = "same",
      name = "trd_l1",
      activation = "relu"
    ))


  merged_daily_trd <- keras::k_concatenate(c(input_daily, trd_l1))

  merged_l3 <- merged_daily_trd |>
    keras::bidirectional(keras::layer_gru(
      units = rec_units,
      name = "merged_l3",
      activation = "relu"
    ))

  merged_l4 <- keras::k_concatenate(c(merged_l3, input_baseline))

  dense_l6 <- merged_l4 |>
    keras::layer_dense(
      name = "dense_l5",
      units = rec_units,
      activation = "relu"
    ) |>
    keras::layer_dense(
      name = "dense_l6",
      units = rec_units,
      activation = "relu"
    )

# Output ----------------------------------------------------------

  out <- dense_l6 %>%
    keras::layer_dense(name = "out", units = 3, activation = "sigmoid")



# Model -----------------------------------------------------------

  model <- keras::keras_model(
    inputs = c(
      input_baseline = input_baseline,
      input_daily = input_daily,
      input_trd = input_trd
    ),
    outputs = out
  )

  model %>%
    compile(
      optimizer = keras::optimizer_adam(
        clipnorm = 0.1,
        clipvalue = 0.1,
        amsgrad = TRUE
      ),
      loss = loss_categorical_crossentropy(),
      metrics = "accuracy"
    )

  model
}

define_keras_model <- function(
    rec_units = 32,
    dense_unit = 16,
    inner_do = 0.5,
    rec_do = 0.5
) {


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
      dropout = inner_do,
      recurrent_dropout = rec_do,
      name = "trd_l1",
      activation = "relu"
    ))


  merged_daily_trd <- keras::k_concatenate(c(input_daily, trd_l1)) |>
    keras::k_batch_normalization()


  merged_l3 <- merged_daily_trd |>
    keras::bidirectional(keras::layer_gru(
      units = rec_units,
      dropout = inner_do,
      recurrent_dropout = rec_do,
      name = "merged_l3",
      activation = "relu"
    ))

  merged_l4 <- keras::k_concatenate(c(merged_l3, input_baseline)) |>
    keras::k_batch_normalization()


  dense_l6 <- merged_l4 |>
    keras::layer_dense(
      name = "dense_l5",
      units = rec_units,
      activation = "relu"
    ) |>
    keras::layer_dropout(inner_do) |>
    keras::k_batch_normalization() |>
    keras::layer_dense(
      name = "dense_l6",
      units = rec_units,
      activation = "relu"
    ) |>
    keras::layer_dropout(inner_do) |>
    keras::k_batch_normalization()

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



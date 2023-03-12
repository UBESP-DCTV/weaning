define_keras_model <- function(
    rec_units = 32,
    dense_units = 16,
    input_do = 0.1,
    inner_do = 0,
    rec_do = 0.2,
    lr = 0.001,
    crnn_kernel_size = 8
) {


# custom layers ---------------------------------------------------



# inputs ----------------------------------------------------------
  input_baseline <- keras::layer_input(
    name = "input_baseline",
    shape = c(7)
  ) |>
    keras::layer_activation_relu() |>
    # keras::layer_dropout(input_do) |>
    keras::layer_batch_normalization()

  input_daily <- keras::layer_input(
    name = "input_daily",
    shape = c(NA, 5)
  ) |>
    keras::layer_activation_relu() |>
    # keras::layer_dropout(input_do) |>
    keras::layer_batch_normalization()

  input_trd <- keras::layer_input(
    name = "input_trd",
    shape = c(1440, NA, 21)
  ) |>
    keras::layer_activation_relu() |>
    # keras::layer_dropout(input_do) |>
    keras::layer_batch_normalization()


# network ---------------------------------------------------------


  trd_l1 <- input_trd |>
    # keras::bidirectional(keras::layer_conv_lstm_1d(
    #   filters = rec_units,
    #   kernel_size = crnn_kernel_size,
    #   padding = "same",
    #   dropout = rec_do,
    #   recurrent_dropout = rec_do,
    #   name = "trd_l1.1",
    #   return_sequences = TRUE
    # )) |>
    # keras::bidirectional(keras::layer_conv_lstm_1d(
    #   filters = rec_units,
    #   kernel_size = crnn_kernel_size,
    #   padding = "same",
    #   dropout = rec_do,
    #   recurrent_dropout = rec_do,
    #   name = "trd_l1.2",
    #   return_sequences = TRUE
    # )) |>
    keras::bidirectional(keras::layer_conv_lstm_1d(
      filters = rec_units,
      kernel_size = crnn_kernel_size,
      padding = "same",
      dropout = rec_do,
      recurrent_dropout = rec_do,
      name = "trd_l1.3"
    ))


  merged_daily_trd <- keras::k_concatenate(c(input_daily, trd_l1)) |>
    keras::layer_batch_normalization()

  merged_l3 <- merged_daily_trd |>
    # keras::bidirectional(keras::layer_gru(
    #   units = rec_units,
    #   dropout = rec_do,
    #   recurrent_dropout = rec_do,
    #   name = "merged_l3.1",
    #   return_sequences = TRUE
    # )) |>
    # keras::bidirectional(keras::layer_gru(
    #   units = rec_units,
    #   dropout = rec_do,
    #   recurrent_dropout = rec_do,
    #   name = "merged_l3.2",
    #   return_sequences = TRUE
    # )) |>
    keras::bidirectional(keras::layer_gru(
      units = rec_units,
      dropout = rec_do,
      recurrent_dropout = rec_do,
      name = "merged_l3.3"
    ))

  merged_l4 <- keras::k_concatenate(c(merged_l3, input_baseline)) |>
    keras::layer_batch_normalization()

  dense_l6 <- merged_l4 |>
    keras::layer_dense(
      name = "dense_l5",
      units = dense_units,
      activation = "relu"
    ) |>
    keras::layer_dropout(inner_do) |>
    keras::layer_batch_normalization() |>
    keras::layer_dense(
      name = "dense_l6",
      units = dense_units,
      activation = "relu"
    ) |>
    keras::layer_dropout(inner_do) |>
    keras::layer_batch_normalization()

# Output ----------------------------------------------------------

  out <- dense_l6 %>%
    keras::layer_dense(name = "out", units = 3, activation = "softmax")



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
        learning_rate = lr,
        # weight_decay = 1e-2,
        # global_clipnorm = 1e-2,
        # clipvalue = 1e-2,
        amsgrad = TRUE
      ),
      loss = loss_categorical_crossentropy(),
      metrics = "accuracy"
    )

  model
}



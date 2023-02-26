define_keras_model <- function() {


# custom layers ---------------------------------------------------

  layer_rescale_1d <- keras::new_layer_class(
    classname = "rescale1d",

    initialize = function(scalars) {
      super$initialize()
      self$scalars <- scalars
    },

    build = function(input_shape) {
      input_dim <- input_shape[length(input_shape)]
      self$W <- tf$constant(self$scalars)
    },

    call = function(inputs) {
      self$W <- tf$cast(self$W, inputs$dtype)
      tf$multiply(inputs, self$W)
    }
  )





# inputs ----------------------------------------------------------
  input_baseline <- keras::layer_input(
    name = "input_baseline",
    shape = c(7)
  )

  input_baseline_normalized <- input_baseline |>
    layer_rescale_1d(c(2, 2, 92, 30, 90, 120, 8)) |>
    layer_batch_normalization()

  input_daily <- keras::layer_input(
    name = "input_daily",
    shape = c(NA, 5)
  )

  input_daily_normalized <- input_daily |>
    layer_rescale_1d(c(14, 8, 160, 95)) |> stop("should be 5?!")
    layer_batch_normalization()


  input_trd <- keras::layer_input(
    name = "input_trd",
    shape = c(1440, NA, 21)
  )

  input_trd_normalized <- input_trd |>
    layer_rescale_1d(c(
      100, 20, 24, 20, 800, 100, 20, 800  , 20 , 40  ,
       40, 35,  3, 50,  40,  10, 100,  3.9,  2.1, 2.1,
      330, 20
    )) |> stop("should be 21?!")
    layer_batch_normalization()



# network ---------------------------------------------------------


  trd_l1 <- input_trd_normalized %>%
    keras::bidirectional(keras::layer_conv_lstm_1d(
      filters = 64,
      kernel_size = 1,
      name = "trd_l1",
      activation = "relu"
    ))


  merged_daily_trd <- keras::k_concatenate(c(input_daily_normalized, trd_l1)) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activity_regularization(l1 = 1e-1, l2 = 1e-1)

  merged_l3 <- merged_daily_trd %>%
    keras::bidirectional(keras::layer_gru(
      units = 64,
      name = "merged_l3",
      activation = "relu"
    ))

  merged_l4 <- keras::k_concatenate(c(merged_l3, input_baseline_normalized)) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activity_regularization(l1 = 1e-1, l2 = 1e-1)

  dense_l6 <- merged_l4 %>%
    keras::layer_dense(
      name = "dense_l5",
      units = 32,
      activation = "relu"
    ) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activity_regularization(l1 = 1e-1, l2 = 1e-1) %>%
    keras::layer_dense(
      name = "dense_l6",
      units = 32,
      activation = "relu"
    ) %>%
    keras::layer_batch_normalization() %>%
    keras::layer_activity_regularization(l1 = 1e-1, l2 = 1e-1)

# Output ----------------------------------------------------------

  out <- dense_l6 %>%
    keras::layer_dense(name = "out", units = 3, activation = "sigmoid")



# Model -----------------------------------------------------------

  keras::keras_model(
    inputs = c(
      input_baseline = input_baseline,
      input_daily = input_daily,
      input_trd = input_trd
    ),
    outputs = out
  )
}

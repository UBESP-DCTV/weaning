# renv::use_python()
is_develop <- FALSE
on_cpu <- FALSE

Sys.unsetenv("RETICULATE_PYTHON")
library(reticulate)
reticulate::use_condaenv("tf", required = TRUE)

if (on_cpu) {
  Sys.setenv("CUDA_VISIBLE_DEVICES" = -1)
  reticulate::py_run_string('
# import os
# os.environ["CUDA_VISIBLE_DEVICES"] = "-1"
# ')
}

library(tensorflow)
library(keras)
k <- reticulate::import("keras", convert = TRUE)


library(targets)

n_days <- 10
data_used <- targets::tar_read(trainArraysByDays, branches = n_days)[[1]]
baseline <- data_used[[2]]
daily <- data_used[[3]]
trd <- data_used[[4]]
outcome <- keras::k_one_hot(data_used[[5]], 3L)


here::here("R") |>
  list.files(
    pattern = "keras",
    full.names = TRUE
  ) |>
  lapply(source) |>
  invisible()


# parameters ------------------------------------------------------
epochs <- 10
batch_size <- 32


summary({
  model <- define_keras_model()
})

if (is_develop) {
  model |>
    # keras:::plot.keras.engine.training.Model(
    plot(
      show_shapes = TRUE,
      show_dtype = TRUE,
      expand_nested = TRUE,
      show_layer_activations = TRUE,
      to_file = "topology-full.png"
    )
}

model %>%
  compile(
    optimizer = k$optimizers$Adam(amsgrad = TRUE),
    loss = loss_categorical_crossentropy(),
    metrics = "accuracy"
  )


{
  run_id <- glue::glue(paste0(
    "run_{stringr::str_remove_all(lubridate::now(), '\\\\W')}"
  ))
  # tb_path <- here::here("logs", run_id)
  # tensorboard(tb_path)

  tic <- Sys.time()
  history <- model %>%
    keras::fit(
      x = list(
        input_baseline = baseline,
        input_daily = daily,
        input_trd = trd
      ),
      y = outcome,
      epochs = epochs,
      batch_size  = batch_size,
      validation_split = 0.2
      # callbacks = list(
      #   callback_model_checkpoint(
      #     filepath = "models_epoch-{epoch:02d}.hdf5",
      #     monitor = "val_loss",
      #     mode = "min"
      #   )
      # )
    )
  (toc <- Sys.time() - tic)
}

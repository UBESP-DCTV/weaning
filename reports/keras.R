is_develop <- TRUE
on_cpu <- TRUE

library(reticulate)
reticulate::use_virtualenv("r-reticulate", required = TRUE)


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

here::here("R") |>
  list.files(pattern = "define_keras_model\\.R$", full.names = TRUE) |>
  lapply(source) |>
  invisible()


# parameters ------------------------------------------------------
epochs <- 30
batch_size <- 16


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
    loss = loss_binary_crossentropy(),
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
      x = targets::tar_read(baselineArrays) |>
        abind::abind(along = 0),
      y = targets::tar_read(outArrays) |>
        purrr::map(~.x[nrow(.x), 1L, drop = FALSE]) |>
        abind::abind(along = 0) |>
        purrr::map_dbl(identical, -1L),
      epochs = epochs,
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

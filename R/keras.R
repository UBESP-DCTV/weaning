here::here("R") |>
  list.files(pattern = "define_keras_model\\.R$", full.names = TRUE) |>
  lapply(source) |>
  invisible()

on_cpu <- TRUE

library(targets)
library(reticulate)


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




# parameters ------------------------------------------------------
epochs <- 10
batch_size <- 16


summary({
  model <- define_keras_model()
})


if (FALSE && requireNamespace("deepviz")) {
  deepviz::plot_model(model, to_file = "topology-full.png")
}

model %>%
  compile(
    optimizer = k$optimizers$Adam(amsgrad = TRUE),
    run_eagerly = TRUE,
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
      x = xxxxxxxx,
      epochs = epochs,
      callbacks = list(
        callback_model_checkpoint(
          filepath = "models_epoch-{epoch:02d}.hdf5",
          monitor = "val_loss",
          mode = "min"
        )
      )
    )
  (toc <- Sys.time() - tic)
}

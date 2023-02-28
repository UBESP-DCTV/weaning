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

library(usethis)
library(tensorflow)
library(keras)
k <- reticulate::import("keras", convert = TRUE)


library(targets)

here::here("R") |>
  list.files(
    pattern = "keras",
    full.names = TRUE
  ) |>
  lapply(source) |>
  invisible()


# parameters ------------------------------------------------------
n_days <- 6
k_folds <- 5
epochs <- 10
batch_size <- 64

data_used <- targets::tar_read(
  trainArraysByDays,
  branches = n_days
)[[1]]

ids <- data_used[[1]]
baseline <- data_used[[2]] / 500
daily <- data_used[[3]] / 500
trd <- data_used[[4]] / 1000
outcome <- data_used[[5]]


fold_id <- sample(rep(seq_len(k_folds), length.out = length(ids)))
k_scores <- numeric(k_folds)
k_histories <- vector("list", k_folds)
k_time <- vector("list", k_folds)

run_id <- glue::glue(paste0(
  "{stringr::str_remove_all(lubridate::now(), '\\\\W')}_run"
))

for (i in seq_len(k_folds)) {
  ui_todo("Processing fold {ui_value(i)}/{k_folds}...")
  are_in_val <- ids[fold_id == i]
  are_in_train <- ids[fold_id != i]

  baseline_train <- baseline[are_in_train, ]
  daily_train <- daily[are_in_train, , ]
  trd_train <- trd[are_in_train, , , ]
  x_train <- list(
    input_baseline = baseline_train,
    input_daily = daily_train,
    input_trd = trd_train
  )
  y_train <- outcome[are_in_train] |>
    keras::k_one_hot(num_classes = 3)

  baseline_val <- baseline[are_in_val, ]
  daily_val <- daily[are_in_val, , ]
  trd_val <- trd[are_in_val, , , ]
  x_val <- list(
    input_baseline = baseline_val,
    input_daily = daily_val,
    input_trd = trd_val
  )
  y_val <- outcome[are_in_val] |>
    keras::k_one_hot(num_classes = 3)

  model <- define_keras_model()

  tic <- Sys.time()
  k_histories[[i]] <- model %>%
    keras::fit(
      x = x_train,
      y = y_train,
      epochs = epochs,
      batch_size  = batch_size,
      verbose = 0
    )
  k_time[[i]] <- round(Sys.time() - tic, 2)

  res <- model |>
    keras::evaluate(x_val, y_val, verbose = 0)

  k_scores[[i]] <- res[["accuracy"]]
  ui_done(
    "Validation accuracy: {ui_value(round(100 * k_scores[[i]], 2))}%."
  )
  (round(Sys.time() - tic, 2))
}

run <- list(
  k_scores = k_scores,
  k_histories = k_histories,
  k_time = k_time
)
ui_info(paste0(
  "Mean k-fold validation accuracy: ",
  "{ui_value(round(100 * mean(run[['k_scores']]), 2))}%."
))
ui_info("Overall training time: ")
print(sum(unlist(run[["k_time"]])))

readr::write_rds(run, here::here("runs", paste0(run_id, ".rds")))



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

{
  # tb_path <- here::here("logs", run_id)
  # tensorboard(tb_path)

}

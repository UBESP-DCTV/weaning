# Setup -----------------------------------------------------------
options(tidyverse.quiet = TRUE)
Sys.unsetenv("RETICULATE_PYTHON")
seed <- 1234
on_cpu <- TRUE
is_develop <- FALSE
verbose <- 1

library(here)
library(lubridate)
library(targets)
library(tidyverse)
library(usethis)

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
tf <- reticulate::import("tensorflow", convert = TRUE)

list.files(here("R"), pattern = "keras", full.names = TRUE) |>
  lapply(source) |>
  invisible()

# Parameters ------------------------------------------------------
run_id <- str_remove_all(now(), '\\W') |> paste0("_run")

k_folds <- 1
epochs <- 50
batch_size <- 16

lr = 1e-3


rec_units = 32
crnn_kernel_size = 1
dense_units = 16


input_do = 0
inner_do = 0.4
rec_do = 0.4



# Global variables ------------------------------------------------
k_scores <- tibble::tibble(
  fold = integer(),
  epochs = integer(),
  set = character(), # train, validation
  loss = numeric(),
  accuracy = numeric()
)
k_histories <- vector("list", k_folds)
k_time <- vector("list", k_folds)
k_sets <- vector("list", k_folds)

# Data ------------------------------------------------------------
ids_trval <- tar_read(idsTrVal)
db_trval <- tar_read(dbTrVal)

ids_test <- tar_read(idsTest)
db_test <- tar_read(dbTest)

set.seed(seed)
fold_id <- ids_trval


# CV-training -----------------------------------------------------
for (i in seq_len(k_folds)) {
  ui_todo("Processing fold {ui_value(i)}/{k_folds}...")
  ui_todo("Preparing train and test generators")
  ids_in_val <- ids_test
  ids_in_train <- ids_trval

  db_tr <- db_trval

  means_baseline <- get_means(db_tr, "baseline")
  means_daily <- get_means(db_tr, "daily")
  means_trd <- get_means(db_tr, "trd")
  sd_baseline <- get_sd(db_tr, "baseline")
  sd_daily <- get_sd(db_tr, "daily")
  sd_trd <- get_sd(db_tr, "trd")

  k_sets[[i]] <- list(
    current_val_fold = i,
    ids_in_train = ids_in_train,
    ids_in_val = ids_in_val,
    means_baseline = means_baseline,
    means_daily = means_daily,
    means_trd = means_trd,
    sd_baseline = sd_baseline,
    sd_daily = sd_daily,
    sd_trd = sd_trd
  )

  db_tr_scaled <- db_tr |>
    normalize_baseline(means_baseline, sd_baseline) |>
    normalize_daily(means_daily, sd_daily) |>
    normalize_trd(means_trd, sd_trd)

  tr_generator <- create_batch_generator(db_tr_scaled, batch_size)
  tr_n_batches <- db_tr |>
    purrr::map_int(~ceiling(length(.x[["ids"]]) / batch_size)) |>
    sum()
  rm(db_tr)

  db_val <- db_test |>
    filter_db_ids(ids_in_val) |>
    normalize_baseline(means_baseline, sd_baseline) |>
    normalize_daily(means_daily, sd_daily) |>
    normalize_trd(means_trd, sd_trd)
  val_generator <- create_batch_generator(db_val, batch_size)
  val_n_batches <- db_val |>
    purrr::map_int(~ceiling(length(.x[["ids"]]) / batch_size)) |>
    sum()
  rm(db_val)
  ui_done("Generators ready.")


  ui_todo("Training in progress...")
  model <- define_keras_model(
    rec_units = rec_units,
    dense_units = dense_units,
    input_do = input_do,
    inner_do = inner_do,
    rec_do = rec_do,
    lr = lr,
    crnn_kernel_size = crnn_kernel_size
  )

  tic <- Sys.time()
  k_histories[[i]] <- model %>%
    keras::fit(
      x = tr_generator,
      steps_per_epoch = tr_n_batches,
      validation_data = val_generator,
      validation_steps = val_n_batches,
      epochs = epochs,
      verbose = verbose,
      callbacks = list(
        callback_model_checkpoint(
          filepath = "models_epoch-{epoch:02d}_acc_{val_accuracy:.2f}.hdf5"
        )
      )
    )
  ui_done("Training done.")
  (k_time[[i]] <- round(Sys.time() - tic, 2))
  k_scores <- k_scores |>
    dplyr::bind_rows(tibble::tibble(
      fold = i,
      set = "train", # train, validation
      epochs = seq_len(k_histories[[i]][["params"]][["epochs"]]),
      loss = k_histories[[i]][["metrics"]][["loss"]],
      accuracy = k_histories[[i]][["metrics"]][["accuracy"]]
    )) |>
    dplyr::bind_rows(tibble::tibble(
      fold = i,
      set = "test", # train, validation
      epochs = seq_len(k_histories[[i]][["params"]][["epochs"]]),
      loss = k_histories[[i]][["metrics"]][["val_loss"]],
      accuracy = k_histories[[i]][["metrics"]][["val_accuracy"]]
    ))
  ui_done("Fold {ui_value(i)}/{k_folds} done.")
}

overall_time <- do.call(sum, k_time)
gg <- k_scores |>
  dplyr::mutate(fold = as.factor(fold)) |>
  tidyr::pivot_longer(c(loss, accuracy)) |>
  ggplot(aes(epochs, value, colour = set, fill = set)) +
  geom_smooth() +
  geom_point() +
  facet_grid(name ~., scales = "free_y") +
  scale_x_continuous(breaks = seq_len(max(epochs))) +
  theme_bw() +
  theme(legend.position = "top") +
  labs(
    subtitle = paste0(
      "Recurrent units: ", rec_units, " - ",
      "Dense units: ", dense_units, " - ",
      "Batch size: ", batch_size, "\n",
      "Recurrent depth: ", 2, " - ",
      "ConvLSTM kernel size: ", crnn_kernel_size, " - ",
      "Dense depth: ", 2, "\n",
      "Input drop-out: ", input_do, "% - ",
      "Internal drop-out: ", inner_do, "% - ",
      "Recurrent drop-out: ", rec_do, "%\n",
      "Batch norm, L1, L2 regularization.\n",
      "Internal activations: ReLU", " - ",
      "Output activations: softmax", "\n",
      "Optimizer: Adam + AMSgrad (starting lr: 1e-4) - ",
      "CV folds: ", k_folds, "."
    ),
    x = "Epoch",
    y = "Value",
    caption = paste0(
      lubridate::today(), " - ",
      "Training time: ",
      overall_time, " ", attr(overall_time, "units"), "."
    ),
    colour = "Set",
    fill = "Set"
  )

run <- list(
  k_scores = k_scores,
  k_histories = k_histories,
  gg_histories = gg,
  k_time = k_time,
  k_final_scores = dplyr::filter(
    k_scores,
    set == "test", epochs == max(.data[["epochs"]])
  )[["accuracy"]],
  k_sets = k_sets
)
ui_info(paste0(
  "Mean k-fold validation last-epoch accuracy: ",
  "{ui_value(round(100 * mean(run[['k_final_scores']]), 2))}%."
))
ui_info(
  "{ui_field('nan')}: {ui_value(sum(is.nan(k_scores[['loss']])))}."
)
print(do.call(sum, k_time))

readr::write_rds(run, here::here("runs", paste0(run_id, "-final.rds")))

if (interactive()) print(gg)

model |>
  # keras:::plot.keras.engine.training.Model(
  plot(
    show_shapes = TRUE,
    show_dtype = TRUE,
    expand_nested = TRUE,
    show_layer_activations = TRUE,
    to_file = "topology-full-final.png"
  )

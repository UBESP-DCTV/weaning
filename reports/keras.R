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
library(tidyverse)
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



# setting data ----------------------------------------------------
db_full <- targets::tar_read(trainArraysByDays)

ids <- targets::tar_read(pt_ids)
ids_test <- character()
ids_trval <- setdiff(ids, ids_test)

db_test <- db_full |>
  filter_db_ids(ids_test)

db_trval <- db_full |>
  filter_db_ids(ids_trval)

rm(ids, db_full)

# parameters ------------------------------------------------------
verbose <- as.integer(interactive())
k_folds <- 5
fold_id <- sample(rep(seq_len(k_folds), length.out = length(ids_trval)))

epochs <- 10
rec_units = 32
dense_unit = 16
batch_size <- 64

run_id <- glue::glue(paste0(
  "{stringr::str_remove_all(lubridate::now(), '\\\\W')}_run"
))

k_scores <- tibble::tibble(
  fold = integer(),
  epochs = integer(),
  set = character(), # train, validation
  loss = numeric(),
  accuracy = numeric()
)
k_histories <- vector("list", k_folds)
k_time <- vector("list", k_folds)


# ids <- data_used[[1]]
# baseline <- data_used[[2]] / 500
# daily <- data_used[[3]] / 500
# trd <- data_used[[4]] / 1000
# outcome <- data_used[[5]]
#

for (i in seq_len(k_folds)) {
  ui_todo("Processing fold {ui_value(i)}/{k_folds}...")
  ids_in_val <- ids_trval[fold_id == i]
  ids_in_train <- ids_trval[fold_id != i]

  db_tr <- db_trval |>
    filter_db_ids(ids_in_train)

  db_val <- db_trval |>
    filter_db_ids(ids_in_val)

  tr_n_batches <- db_tr |>
    purrr::map_int(~ceiling(length(.x[["ids"]]) / batch_size)) |>
    sum()
  val_n_batches <- db_val |>
    purrr::map_int(~ceiling(length(.x[["ids"]]) / batch_size)) |>
    sum()

  tr_generator <- create_batch_generator(db_tr, batch_size)
  val_generator <- create_batch_generator(db_val, batch_size)

  model <- define_keras_model(
    rec_units = rec_units,
    dense_unit = dense_unit
  )

  tic <- Sys.time()
  k_histories[[i]] <- model %>%
    keras::fit(
      x = tr_generator,
      steps_per_epoch = tr_n_batches,
      validation_data = val_generator,
      validation_steps = val_n_batches,
      epochs = epochs,
      verbose = varbose
    )
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
      set = "validation", # train, validation
      epochs = seq_len(k_histories[[i]][["params"]][["epochs"]]),
      loss = k_histories[[i]][["metrics"]][["val_loss"]],
      accuracy = k_histories[[i]][["metrics"]][["val_accuracy"]]
    ))
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
      "Dense units: ", dense_unit, " - ",
      "Batch size: ", batch_size, "\n",
      "Recurrent depth: ", 1, " - ",
      "Dense depth: ", 2, "\n",
      "Input drop-out: ", 0, "% - ",
      "Internal drop-out: ", 0, "% - ",
      "Recurrent drop-out: ", 0, "%\n",
      "Internal activations: ReLU", " - ",
      "Optimizer: Adam + AMSgrad."
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
    set == "validation", epochs == max(.data[["epochs"]])
  )[["accuracy"]]
)
ui_info(paste0(
  "Mean k-fold validation last-epoch accuracy: ",
  "{ui_value(round(100 * mean(run[['k_final_scores']]), 2))}%."
))
ui_info(
  "{ui_field('nan')}: {ui_value(sum(is.nan(k_scores[['loss']])))}."
)
print(do.call(sum, k_time))

readr::write_rds(run, here::here("runs", paste0(run_id, ".rds")))

if (interactive()) print(gg)

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

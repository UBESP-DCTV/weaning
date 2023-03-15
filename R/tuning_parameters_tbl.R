tuning_parameters_tbl <- function(runs_dir_path = here::here("runs")) {

  runs_dir_path |>
    list.files("run\\.rds$", full.names = TRUE) |>
    purrr::set_names() |>
    purrr::map_dfr(~{

      run <- readr::read_rds(.x)
      par <- run[["gg_histories"]][["labels"]][["subtitle"]]

      val <- run[["k_scores"]] |>
        dplyr::filter(set == "validation") |>
        dplyr::with_groups(
          epochs,
          dplyr::summarise,
          set = unique(set),
          loss = mean(loss),
          acc = mean(accuracy)
        ) |>
        dplyr::filter(loss == min(loss))

      tr <- run[["k_scores"]] |>
        dplyr::filter(set == "train") |>
        dplyr::with_groups(
          epochs,
          dplyr::summarise,
          set = unique(set),
          loss = mean(loss),
          acc = mean(accuracy)
        )

      val |>
        dplyr::bind_rows(
          dplyr::filter(tr, epochs == val[["epochs"]])
        ) |>
        dplyr::mutate(
          rec_units = par |>
            stringr::str_extract("(?<=Recurrent units: )\\d+"),
          dense_units = par |>
            stringr::str_extract("(?<=Dense units: )\\d+"),
          batch_size = par |>
            stringr::str_extract("(?<=Batch size: )\\d+"),
          rec_depth = par |>
            stringr::str_extract("(?<=Recurrent depth: )\\d+"),
          crnn_kernel = par |>
            stringr::str_extract("(?<=ConvLSTM kernel size: )\\d+"),
          in_do = par |>
            stringr::str_extract("(?<=Input drop-out: )[\\d.]+"),
          rec_do = par |>
            stringr::str_extract("(?<=Recurrent drop-out: )[\\d.]+"),
          out_do = par |>
            stringr::str_extract("(?<=Internal drop-out: )[\\d.]+"),
          time = mean(unlist(run$k_time)),
          unit = purrr::map_chr(run[["k_time"]], attr, "units") |>
            unique() |>
            paste(collapse = "/")
        )
    },
    .id = "file") |>
    dplyr::mutate(
      file = basename(file)
    ) |>
    tidyr::pivot_wider(names_from = set, values_from = c(loss, acc)) |>
    dplyr::mutate(file = dplyr::row_number()) |>
    gt::gt() |>
    gt::fmt_number(
      columns = c(
        loss_validation, loss_train, acc_validation, acc_train
      ),
      decimals = 2
    ) |>
    gt::cols_label(
      file = "Candidate model",
      epochs = "Epochs",
      rec_units = "Recurrent units",
      dense_units = "Dense units",
      batch_size = "Batch size",
      rec_depth = "Recurrent depth",
      crnn_kernel = "Convolutional RNN Kernel",
      in_do = "Dropout rate input layer",
      rec_do = "Dropout rate recurrent layer",
      out_do = "Dropout rate output layer",
      time = "Time",
      unit = "(unit)",
      loss_validation = "Validation loss",
      loss_train = "Train loss",
      acc_validation = "Validation accuracy (%)",
      acc_train = "Train accuracy (%)"
    )
}

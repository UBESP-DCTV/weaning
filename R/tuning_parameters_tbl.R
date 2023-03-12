here::here("runs") |>
  list.files("rds$", full.names = TRUE) |>
  purrr::set_names() |>
  purrr::map_dfr(~{

    run <- readr::read_rds(.x)
    par <- run[["gg_histories"]][["labels"]][["subtitle"]]

    run[["k_scores"]] |>
    dplyr::filter(set == "validation") |>
    dplyr::with_groups(
      epochs,
      dplyr::summarise,
      loss = mean(loss),
      acc = mean(accuracy)
    ) |>
    dplyr::filter(loss == min(loss)) |>
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
      input_do = par |>
        stringr::str_extract("(?<=Input drop-out: )[\\d.]+"),
      internal_do = par |>
        stringr::str_extract("(?<=Internal drop-out: )[\\d.]+")
    )
  }, .id = "file") |>
  dplyr::mutate(file = basename(file))

# layer_rescale_1d <- keras::new_layer_class(
#   classname = "rescale1d",
#
#   initialize = function(scalars) {
#     super$initialize()
#     self$scalars <- scalars
#   },
#
#   build = function(input_shape) {
#     input_dim <- input_shape[length(input_shape)]
#     self$W <- tf$constant(self$scalars)
#   },
#
#   call = function(inputs) {
#     self$W <- tf$cast(self$W, inputs$dtype)
#     tf$multiply(inputs, self$W)
#   }
# )


create_batch_generator <- function(
    db,
    batch_size = 16,
    seed = 1
) {
  # start iterator
  max_n_batches <- db |>
    purrr::map_int(~ceiling(length(.x[["ids"]]) / batch_size))
  max_daily_records <- purrr::map(db, ~length(.x[["ids"]]))
  n_days <- length(max_n_batches)
  current_n_batches <- rep(1L, n_days)

  i <- 1

  # return an iterator function
  function() {
    # reset iterator if already seen all data
    if (current_n_batches[[i]] > max_n_batches[[i]]) {
      current_n_batches[[i]] <<- 1L
      i <<- i + 1L
    }
    if (i > n_days) i <<- 1

    first <- 1 + (current_n_batches[[i]] - 1L) * batch_size
    last <- min(
      first + current_n_batches[[i]] * batch_size - 1L,
      max_daily_records[[i]]
    )

    out <- db[[i]][["out"]][first:last, drop = FALSE]
    y <- as.array(out, size = c(length(out), 3L)) |>
      keras::k_one_hot(num_classes = 3)

    res <- list(
      x = list(
        input_baseline = db[[i]][["baseline"]][first:last, , drop = FALSE],
        input_daily = db[[i]][["daily"]][first:last, , , drop = FALSE],
        input_trd = db[[i]][["trd"]][first:last, , , , drop = FALSE]
      ),
      y = y
    )
    # return current batch

    current_n_batches[[i]] <<- current_n_batches[[i]] + 1L
    res
  }
}



filter_db_ids <- function(db, ids) {
  db_res <- db |>
    purrr::map(~{
      ids <- intersect(.x[[1]], ids)
      list(
        ids = ids,
        baseline = .x[[2]][ids, , drop = FALSE],
        daily = .x[[3]][ids, , , drop = FALSE],
        trd = .x[[4]][ids, , , , drop = FALSE],
        out = .x[[5]][ids, drop = FALSE]
      )
    })
  daily_records <- db_res |>
    purrr::map_int(~length(.x[["ids"]]))

  # return nontrivial only
  db_res[daily_records != 0]
}


get_means <- function(db, what) {
  db_what <- purrr::map(db, what)
  do.call(
    rbind,
    switch(what,
           baseline = db_what,
           daily = db_what |>
             purrr::map(~keras::array_reshape(.x, dim = c(prod(dim(.x)[-3]), 5))),
           trd = db_what |>
             purrr::map(~keras::array_reshape(.x, dim = c(prod(dim(.x)[-4]), 21)))
    )
  ) |>
    colMeans()
}
get_sd <- function(db, what) {
  db_what <- purrr::map(db, what)
  do.call(
    rbind,
    switch(what,
           baseline = db_what,
           daily = db_what |>
             purrr::map(~keras::array_reshape(.x, dim = c(prod(dim(.x)[-3]), 5))),
           trd = db_what |>
             purrr::map(~keras::array_reshape(.x, dim = c(prod(dim(.x)[-4]), 21)))
    )
  ) |>
    apply(2, sd)
}

normalize_baseline <- function(db, means, sds) {
  purrr::map(db, ~{
    for (i in seq_along(means)) {
      .x[["baseline"]][, i] <- (
        .x[["baseline"]][, i] - means[i]
      ) /
        sds[i]
    }
    .x
  })
}


normalize_daily <- function(db, means, sds) {
  purrr::map(db, ~{
    for (i in seq_along(means)) {
      .x[["daily"]][, , i] <- (
        .x[["daily"]][, , i] - means[i]
      ) /
        sds[i]
    }
    .x
  })
}


normalize_trd <- function(db, means, sds) {
  purrr::map(db, ~{
    for (i in seq_along(means)) {
      .x[["trd"]][, , , i] <- (
        .x[["trd"]][, , , i] - means[i]
      ) /
        sds[i]
    }
    .x
  })
}


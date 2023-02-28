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
    data_listday,
    batch_size = 16,
    seed = 1
) {
  # start iterator
  max_n_batches <- data_listday |>
    purrr::map_int(~ceiling(length(.x[["ids"]]) / batch_size))
  max_daily_records <- purrr::map(data_listday, ~length(.x[["ids"]]))
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

    res <- list(
      x = list(
        input_baseline = data_listday[["baseline"]][first:last, ],
        input_daily = data_listday[["daily"]][first:last, , ],
        input_trd = data_listday[["trd"]][first:last, , , ]
      ),
      y = data_listday[["out"]][first:last] |>
        keras::k_one_hot(num_classes = 3)
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
        baseline = .x[[2]][ids, ],
        daily = .x[[3]][ids, , ],
        trd = .x[[4]][ids, , , ],
        out = .x[[5]][ids]
      )
    })
  daily_records <- db_res |>
    purrr::map_int(~length(.x[["ids"]]))

  # return nontrivial only
  db_res[daily_records != 0]
}





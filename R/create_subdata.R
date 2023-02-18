create_subdata <- function(ids, baseline, daily, trd, outcome, n_days = 2) {
  stopifnot(length(ids) == length(baseline))
  stopifnot(length(ids) == length(daily))
  stopifnot(length(ids) == length(trd))
  stopifnot(length(ids) == length(outcome))

  baseline <- purrr::set_names(baseline, ids)
  daily <- purrr::set_names(daily, ids)
  trd <- purrr::set_names(trd, ids)
  outcome <- purrr::set_names(outcome, ids)

  outcome_relevant_days <- outcome |>
    purrr::map(~{
      .x[seq_len(n_days), 1L, drop = FALSE] |>
        remove_lasts_value()
    })

  outcome_days <- purrr::map_int(outcome_relevant_days, ~dim(.x)[[1]])
  daily_days <- purrr::map_int(daily, ~dim(.x)[[1]])
  trd_days <- purrr::map_int(trd, ~dim(.x)[[2]])

  max_common_days <- pmin(daily_days, trd_days, outcome_days)
  have_enough_day <- max_common_days >= n_days

  current_ids <- ids[have_enough_day]
  current_baseline <- baseline[have_enough_day]
  current_outcome <- outcome[have_enough_day] |>
    purrr::map2(
      max_common_days[have_enough_day],
      ~.x[seq_len(.y), 1L, drop = FALSE])

  current_trd <- trd[have_enough_day] |>
    purrr::map2(
      current_outcome,
      ~.x[, seq_len(nrow(.y)), , drop = FALSE]
    )

  current_daily <- daily[have_enough_day] |>
    purrr::map2(
      current_outcome,
       ~.x[seq_len(nrow(.y)), , drop = FALSE]
    )

  list(
    ids = current_ids,
    baseline = abind::abind(current_baseline, along = 0L),
    daily = abind::abind(current_daily, along = 0L),
    trd = abind::abind(current_trd, along = 0L),
    n_patients = length(current_daily),
    n_days = n_days
  )
}

get_max_day <- function(daily, trd, outcome) {
  max_trd <- max(purrr::map_int(trd, ~dim(.x)[[2]]))
  max_daily <- max(purrr::map_int(daily, ~dim(.x)[[1]]))
  max_outcome <- outcome |>
    purrr::map_int(
      ~dim(remove_lasts_value(.x[, 1, drop = FALSE]))[[1]]
    ) |>
    max()
  min(max_trd, max_daily, max_outcome)
}


remove_lasts_value <- function(x, value = -1) {
  # is_value <- x[, 1] == value
  # if (!is_value[length(is_value)]) return(x)
  #
  #   stop("implementing faster procedure")

  lenx <- length(x)
  if (lenx == 0) return(x)
  if (x[[lenx, 1L]] == value) return(
    remove_lasts_value(x[-lenx, 1L, drop = FALSE], value = value)
  )
  x
}

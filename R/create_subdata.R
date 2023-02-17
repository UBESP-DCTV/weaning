create_subdata <- function(ids, baseline, daily, trd, outcome, n_days = 2) {
  stopifnot(length(ids) == length(baseline))
  stopifnot(length(ids) == length(daily))
  stopifnot(length(ids) == length(trd))
  stopifnot(length(ids) == length(outcome))

  current_daily <- daily |>
    purrr::set_names(ids) |>
    purrr::keep(~dim(.x)[[1]] >= n_days) |>
    purrr::map(~.x[seq_len(n_days), , drop = FALSE])

  current_trd <- trd |>
    purrr::set_names(ids) |>
    purrr::keep(~dim(.x)[[2]] >= n_days) |>
    purrr::map(~.x[, seq_len(n_days), , drop = FALSE])

  current_outcome <- outcome |>
    purrr::set_names(ids) |>
    purrr::keep(~dim(.x)[[1]] >= n_days) |>
    purrr::map(~.x[seq_len(n_days), 1L, drop = FALSE])

  list(
    ids = ids[names(current_daily)],
    baseline = abind::abind(baseline, along = 0L),
    daily = abind::abind(current_daily, along = 0L),
    trd = abind::abind(current_trd, along = 0L),
    n_patients = length(current_daily),
    n_days = n_days
  )
}

get_max_day <- function(daily) {
  purrr::map_int(daily, ~dim(.x)[[1]]) |>
    max()
}

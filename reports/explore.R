library(tidyverse)
library(targets)

tar_read(pt_names) |> dplyr::glimpse()
tar_read(pt_registry) |> dplyr::glimpse()


tar_read(pt_registry) |>
  add_sbt() |>
  dplyr::select(estubato, sbt) |>
  table(useNA = "always")

import_registry

tar_read(weaning_days)$esito |> table(useNA = "always")



fake <- tribble(
  ~a, ~b,
  1, TRUE,
  1, FALSE,
  2, FALSE,
  2, FALSE
)

fake |>
  group_by(a) |>
  mutate(
    c = case_when(
      lag(b, default = FALSE) ~ 1,
      TRUE ~ 2
    )
  )


tar_read(pt_registry) |>
  group_by(id_univoco) |>
  filter(data_lettura == min(data_lettura)) |>
  # ungroup() |>
  select(id_univoco, susp_tot, estubato) |>
  filter(susp_tot == 12, estubato) |>
  mutate(
    a = !lag(.data[["estubato"]], default = TRUE),
    b = !lag(.data[["estubato"]], default = FALSE)
  )

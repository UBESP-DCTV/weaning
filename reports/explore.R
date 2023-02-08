library(tidyverse)
library(targets)

tar_read(pt_names) |> dplyr::glimpse()
tar_read(pt_registry) |> dplyr::glimpse()


tar_read(pt_registry) |>
  select(id_univoco, esito) |>
  group_by(esito, id_univoco) |>
  tally() |> #summarise(sum(n)) #|> pull(n) |> sum()
pivot_wider(
  names_from = esito,
  values_from = n,
  values_fill = 0
) |>
  ggplot(aes(x = Fallito, y = Successo)) +
  geom_count(aes(colour = ..n.., size = ..n..)) +
  labs( title = "Numero di tentativi per paziente")


tar_read(weaningsLOG)$time


# patient_history_plot(
#   ,
#   tar_read(weaningsLOG),
#   tar_read(pt_names),
#   tar_read(pt_registry),
#   "TS",
#   12
# )


get_gross_minutes <- function(hm) {
  60 * lubridate::hour(hm) + lubridate::minute(hm)
}
get_gross_minutes(tar_read(weaningsTRD)$ora) |>
  range()

names(tar_read(weaningsTRD))

tar_read(weaningsTRD)$et_co2_percent




res <- targets::tar_read(weaningsTRD) |>
  create_pt_trd("AN001")

res_arr <- purrr::map(
    c(0, seq_len(max(res[["day"]], na.rm = TRUE))),
    ~ res[res[["day"]] == .x, , drop = FALSE] |>
      as.matrix()
  ) |>
  abind::abind(along = 1.5)

str(res_arr)


  # res <- dplyr::filter(.data[["id_univoco"]] == "AN001") |>
  # dplyr::arrange(.data[["date"]], .data[["ora"]]) |>
  # dplyr::select(
  #   -dplyr::all_of(c(
  #     "id_univoco", "folder", "file", "id_pat", "stress_index",
  #     "et_co2_percent"
  #   ))
  # ) |>
  # dplyr::mutate(
  #   day = as.integer(
  #     .data[["date"]] - min(.data[["date"]], na.rm = TRUE)
  #   ),
  #   minute = get_gross_minutes(.data[["ora"]])
  # ) |>
  # dplyr::select(-dplyr::all_of(c("date", "ora"))) |>
  # dplyr::relocate(
  #   dplyr::all_of(c("day", "minute")),
  #   .before = dplyr::everything()
  # ) |>
  # tidyr::complete(
  #   day = c(0, seq_len(max(.data[["day"]], na.rm = TRUE))),
  #   fill = list(minute = 0)
  # ) |>
  # dplyr::group_by(.data[["day"]]) |>
  # tidyr::complete(minute = 0:1439) |>
  # dplyr::arrange(.data[["day"]], .data[["minute"]])
  #
  # res[is.na(res)] <- -99




abind::abind(
  list(
    matrix(1:4, 2, 2),
    matrix(5:8, 2, 2)
  ),
  along = 1.8
)






tar_read(weaningsTRD)






library(tidyverse)
library(targets)

trd <- tar_read(weaningsTRD)
log <- tar_read(weaningsLOG)




log |> filter(is.na(id_pat))

trd |>
  janitor::get_dupes(-file)

trd <- qs::qread(here::here("_targets/objects/weaningsTRD"))


trd |>
  purrr::map_dbl(~mean(is.na(.x))) |>
  as.list() |>
  as_tibble() |>
  pivot_longer(cols = everything()) |>
  mutate(value = round(100 * value)) |>
  ggplot(aes(x=reorder(name, desc(value)), value, fill = name)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_bw() +
  labs(
    x = "Variabile",
    y = "proporzione missing",
    fill = "Variabili"
  ) +
  theme(legend.position = "none")

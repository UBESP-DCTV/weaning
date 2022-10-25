library(targets)
library(tarchetypes)
library(here)
library(future)
if(availableCores("multicore") > 2L) {
  plan(multicore(workers = availableCores("multicore") - 1L))
} else {
  plan(multisession(workers = availableCores() - 1L))
}
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(result) to view the results.

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.
list.files(here("R"), pattern = "\\.R$", full.names = TRUE) |>
  lapply(source) |> invisible()

# Set target-specific options such as packages.
tar_option_set(packages = "tidyverse")

# End this file with a list of target objects.
list(

  tar_target(
    weaningFolder,
    get_input_data_path(),
    format = "file"
  ),
  tar_target(
    weaningsTRD,
    import_folders(weaningFolder, "TRD") |>
      fix_wrong_hours(),
    packages = "furrr",
    format = "qs"
  ),

  tar_target(problematicDupes, get_problematic_dupes(weaningsTRD)),

  tar_target(
    weaningsLOG,
    import_folders(weaningFolder, "LOG"),
    packages = "furrr",
    format = "qs"
  ),



  tar_target(ggPatPerCentro, {
    weaningsTRD |>
      distinct(folder, id_pat) |>
      ggplot(aes(forcats::fct_infreq(folder), fill = folder)) +
      geom_bar() +
      labs(
        x = "Centro",
        y = "Numero pazienti"
      ) +
      ggtitle("Numero pazienti per centro") +
      theme_bw() +
      theme(legend.position = "none")
  }),


  tar_target(ggDistAllarmi, {
    weaningsLOG |>
      ggplot(aes(
        forcats::fct_infreq(tipo),
        fill = forcats::fct_infreq(tipo)
      )) +
      geom_bar() +
      ggtitle(
        "Distribuzione dei tipi di allarmi nel database Weanings"
      ) +
      scale_y_log10() +
      theme_bw() +
      labs(
        x = "Tipo allarme",
        y = "N (scala logaritmica)",
        fill = "Tipo allarme"
      ) +
      theme(axis.text.x = element_blank())
  }),


  tar_target(ggMissingTRD, {
    weaningsTRD |>
      purrr::map_dbl(~mean(is.na(.x))) |>
      as.list() |>
      as_tibble() |>
      pivot_longer(cols = everything()) |>
      mutate(
        name = name |>
          stringr::str_replace_all("_", " ") |>
          stringr::str_to_sentence(),
        value = round(100 * value, 2)
      ) |>
      ggplot(aes(x = reorder(name, desc(value)), value, fill = name)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = value), size = 2) +
      coord_flip() +
      theme_bw() +
      labs(
        x = "Variabile",
        y = "Percentuale dati mancanti (%)",
        fill = "Variabili"
      ) +
      theme(legend.position = "none")
  }),


  # compile the report
  tar_render(report, here("reports/report.Rmd")),
  tar_render(trd_log_csv_exploration, here("reports/trd_log_csv_exploration.qmd")),


  tar_target(
    objectToShare,
    list(
      ggPatPerCentro = ggPatPerCentro,
      ggDistAllarmi = ggDistAllarmi,
      ggMissingTRD = ggMissingTRD,
      weaningsTRD = weaningsTRD,
      weaningsLOG = weaningsLOG
    )
  ),

  tar_target(
    shareOutput,
    share_objects(objectToShare),
    format = "file",
    pattern = map(objectToShare)
  )
)

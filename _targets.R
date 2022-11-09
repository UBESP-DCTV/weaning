library(targets)
library(tarchetypes)
library(here)
# library(future)
# if(availableCores("multicore") > 2L) {
#   plan(multicore(workers = availableCores("multicore") - 1L))
# } else {
#   plan(multisession(workers = availableCores() - 1L))
# }

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
    weaningsTRD,{
    import_folders(weaningFolder, "TRD") |>
      fix_wrong_hours() |>
      dplyr::mutate( id_univoco = ifelse(
        test = id_pat <10,
        yes = paste0(folder, "00", id_pat),
        no = paste0(folder, "0", id_pat) ))
    },
    packages = "furrr",
    format = "qs"
  ),

  tar_target(problematicDupes, get_problematic_dupes(weaningsTRD)),

  tar_target(
    weaningsLOG,{
    import_folders(weaningFolder, "LOG") |>
      dplyr::mutate( id_univoco = ifelse(
        test = id_pat <10,
        yes = paste0(folder, "00", id_pat),
        no = paste0(folder, "0", id_pat) ),
        date = data )
    },
    packages = "furrr",
    format = "qs"
  ),

  tar_target(
    pt_names,
    import_patients(),
    format = "qs"
  ),

  tar_target(
    pt_registry,
    import_registry(),
    format = "qs"
  ),

  tar_target(
    weaning_succ, {
    pt_registry %>%
      group_by( id_univoco) %>%
      filter( susp_tot == 12, # criterio 1
              estubato == 1, # criterio 2
              lag(estubato, default = 0) == 0) %>%
      mutate( esito = factor( x = "Successo",
                              levels = c("Fallito", "Successo")))
  }),

  tar_target(
    weaning_fail, {
    weaning_fail <- pt_registry %>%
      group_by( id_univoco) %>%
      filter( susp_tot == 12,
              estubato == 0)  %>%
      mutate( esito = factor( x = "Fallito",
                              levels = c("Fallito", "Successo")))
    }),

  tar_target(
    weaning_days, {
    weaning_succ %>%
      bind_rows(weaning_fail) %>%
      arrange(id_univoco)
  }),

  tar_target(
    weaning_subset, {
    patient_subset <- c("TS012", "BS002", "NO004")
    weaning_subset <- weaning_days %>%
      select(id_univoco, data_lettura, esito, type) %>%
      filter( id_univoco %in% patient_subset) %>%
      group_by(id_univoco) %>%
      arrange(data_lettura)
  }),

  tar_target(
    weaning_log_subset,{
    weaningsLOG %>%
      filter( id_univoco %in% weaning_subset[["id_univoco"]],
              date %in% weaning_subset[["data_lettura"]])
  }),

  tar_target(
    weaning_log_filtered,{
    weaning_log_subset %>%
      filter( id_info %in% c(0, 267, 291, 321, 322),
              !str_detect(informazioni, 'Silenziamento'),
              !str_detect(informazioni, 'Pre-silenziamento') )
  }),

  tar_target(
    weaning_trd_subset,{
      weaningsTRD %>%
        filter( id_univoco %in% weaning_subset[["id_univoco"]],
                date %in% weaning_subset[["data_lettura"]])
    }),

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

  tar_target(ggSmoothReadinessSBT, {
    p <- pt_registry |>
      select(id_univoco, susp_tot, giorno_studio) |>
      ggplot(aes(x = giorno_studio,
                 y = susp_tot)) +
      geom_point(alpha = 0) +
      geom_smooth() +
      labs( title = "Trend di readiness allo SBT",
            subtitle = "all'aumentare dei giorni di ventilazione")
    ggExtra::ggMarginal(p, fill = "light blue")
  }),

  tar_target(ggWeanVariablesAll, {
    weaningsTRD |>
      filter( folder == "BS",
              id_pat == 8) %>%
      filter(date == "2013-11-27") %>%
      select_if( function(x) {!all(is.na(x))} ) %>%
      gather(-ora, key = "var", value = "value") %>%
      ggplot(aes(x = ora,
                 y = value)) +
      geom_point(size = 0.5) +
      facet_wrap(~ var, scales = "free")
  }),

  tar_target(ggWeanVariablesSel, {
    weaningsTRD %>%
      filter( folder == "BS",
              id_pat == 8) %>%
      select(date,
             ora,
             lavoro_respiratorio_del_ventilatore_joule_l,
             lavoro_respiratorio_del_paziente_joule_l,
             pressione_di_fine_esp_cm_h2o,
             press_media_vie_aeree_cm_h2o) %>%
      pivot_longer(cols = 3:6) %>%
      ggplot(aes(x = ora,
                 y = value,
                 color = name)) +
      geom_line() +
      facet_wrap(~date) +
      labs( title = "Weaning TRD plot",
            subtitle = "Paziente BS008",
            y = "")
  }),

  tar_target(patientHistoryPlotTS015, {
    patient_history_plot("TS", 15)
  }),
  tar_target(patientHistoryPlotTS012, {
    patient_history_plot("TS", 12)
  }),
  tar_target(patientHistoryPlotBS002, {
    patient_history_plot("BS", 2)
  }),
  tar_target(patientHistoryPlotNO004, {
    patient_history_plot("NO", 04)
  }),

  tar_target(ggTentativiPerPaziente, {
  weaning_days %>%
    select(id_univoco, esito) %>%
    mutate(i = 1) %>%
    pivot_wider( names_from = esito,
                 values_from = i,
                 values_fn = sum,
                 values_fill = 0) %>%
    ggplot() +
    geom_count( aes( x = Fallito,
                     y = Successo,
                     colour = ..n..)) +
    labs( title = "Numero di tentativi per paziente")
  }),

  tar_target(ggWeaningSubsetDay, {
    plot_trd(weaning_subset, color_files = FALSE)
  }),

  tar_target(ggWeaningSubsetFile, {
    plot_trd(weaning_subset, color_files = TRUE)
  }),

  tar_target(ggWeaningLogSubsetRaw,{
    ggWeaningSubsetFile +
      geom_rug(data = weaning_log_subset,
               aes(x = ora,
                   color = file))
  }),

  tar_target(ggWeaningLogSubsetFiltered,{
    ggWeaningSubsetFile +
      geom_rug(data = weaning_log_filtered,
               aes(x = ora,
                   color = file))
  }),

  tar_target(ggMediaMobileSubset, {
    ggplot(weaning_trd_subset,
           aes( x = ora)) +
      tidyquant::geom_ma(aes(
        y = lavoro_respiratorio_del_ventilatore_joule_l *10),
        n = 30,
        linetype = 1) +
      tidyquant::geom_ma(aes(
        y = lavoro_respiratorio_del_paziente_joule_l *10),
        n = 30,
        color = "dark blue",
        linetype = 1) +
      tidyquant::geom_ma(aes(
        y = pressione_di_fine_esp_cm_h2o),
        n = 30,
        color = "dark green",
        linetype = 1) +
      tidyquant::geom_ma(aes(
        y = press_media_vie_aeree_cm_h2o),
        n = 30,
        color = "dark orange",
        linetype = 1) +
      labs(title = "Moving averages on weanings TRD",
           x = "", y = "") +
      facet_wrap(vars(id_univoco, date))
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
      weaningsLOG = weaningsLOG,
      ggWeanVariablesAll = ggWeanVariablesAll,
      ggWeanVariablesSel = ggWeanVariablesSel,
      patientHistoryPlotTS015 = patientHistoryPlotTS015,
      patientHistoryPlotTS012 = patientHistoryPlotTS012,
      patientHistoryPlotBS002 = patientHistoryPlotBS002,
      patientHistoryPlotNO004 = patientHistoryPlotNO004,
      ggTentativiPerPaziente = ggTentativiPerPaziente,
      ggWeaningLogSubsetFiltered = ggWeaningLogSubsetFiltered,
      ggMediaMobileSubset = ggMediaMobileSubset
    )
  ),

  tar_target(
    shareOutput,
    share_objects(objectToShare),
    format = "file",
    pattern = map(objectToShare)
  )
)

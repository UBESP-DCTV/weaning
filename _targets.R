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
tar_option_set(
  packages = "tidyverse",
  workspace_on_error = TRUE,
  error = "continue"
)

# End this file with a list of target objects.
list(




  # Import data -----------------------------------------------------


  tar_target(
    weaningFolder,
    get_input_data_path(),
    format = "file"
  ),

  tar_target(
    patientsToRemove,
    c(
      "NO021",
      "FE017"  # record tutti riferiti (nei metadati interni) a FE016
    )
  ),

  # tar_target(fileToSkip, c(
  #   "BG003_1119_LOG_2014-03-13_15-53-07",  # BG002
  #   "BS006_459_LOG", "BS006_459_TRD",  # BS005
  #   "BS007_520_TRD",  # BS006
  #   "BS009_900_LOG", "BS009_900_TRD",  # BS012
  #   "CM001_108_LOG", "CM001_108_TRD",  # CM002
  #   "CM001_114_LOG", "CM001_114_TRD",  # CM002
  #   "CM001_118_LOG", "CM001_118_TRD",  # CM002
  #   "CM002_101_LOG", "CM002_101_TRD",  # CM001
  #   "CM002_115_LOG", "CM002_115_TRD",  # CM001
  #   "CM002_115_LOG_2013-08-15_11-01-11",  # CM001
  #   "CM002_115_TRD_2013-08-15_11-01-10",  # CM001
  #   "CM002_115_TRD_2013-08-15_11-02-48", # CM001
  #   "CM002_115_TRD_2013-08-15_11-08-31",  # CM001
  #   "CM002_115_TRD_2013-08-15_11-11-53",  # CM001
  #   "CM002_115_TRD_2013-08-16_11-45-14", # CM001
  #   "CM002_115_TRD_2013-08-16_11-47-02",   # CM001
  #   "CM002_124_TRD_2013-08-17_06-36-41",  # CM001
  #   "CM002_124_TRD_2013-08-17_06-46-39",  # CM001
  #   "CM002_124_TRD_2013-08-17_06-47-52",
  #   "CM002_141_TRD_2013-08-21_09-18-17",
  #   "CM019_1587_TRD", "CM019_1588_TRD", "CM019_1592_TRD", # CM009
  #   "FE002_16_TRD_2013-07-14_16-53-45", # FE001
  #   "FE003_17_TRD",  # FE002
  #   "CM002_124_TRD",  # CM001
  #   "BS016_1084_LOG"  # BS017
  # )),
  #
  # tar_target(PedotToSkip, c(
  #   "BA011_1893_TRD",  # pt baoo10 ma non esiste BA010
  #   "BA011_1894_TRD",  # tra quelli in studio
  #   "BA011_1895_TRD",  # quindi si potrebbe includere
  #   "BS006_459_TRD",  # BS005
  #   "BS007_520_TRD",  # BS006
  #   "BS009_900_TRD",  # BS012
  #   "CM001_108_LOG", "CM001_108_TRD",  # CM002
  #   "CM001_114_LOG", "CM001_114_TRD",  # CM002
  #   "CM001_118_LOG", "CM001_118_TRD",  # CM002
  #   "CM002_101_LOG", "CM002_101_TRD",  # CM001
  #   "CM002_115_LOG", "CM002_115_TRD",  # CM001
  #   "CM002_115_LOG_2013-08-15_11-01-11",  # CM001
  #   "CM002_115_TRD_2013-08-15_11-01-10",  # CM001
  #   "CM002_115_TRD_2013-08-15_11-02-48", # CM001
  #   "CM002_115_TRD_2013-08-15_11-08-31",  # CM001
  #   "CM002_115_TRD_2013-08-15_11-11-53",  # CM001
  #   "CM002_115_TRD_2013-08-16_11-45-14", # CM001
  #   "CM002_115_TRD_2013-08-16_11-47-02",   # CM001
  #   "CM002_124_TRD_2013-08-17_06-36-41",  # CM001
  #   "CM002_124_TRD_2013-08-17_06-46-39",  # CM001
  #   "CM002_124_TRD_2013-08-17_06-47-52",
  #   "CM002_141_TRD_2013-08-21_09-18-17",
  #   "CM019_1587_TRD", "CM019_1588_TRD", "CM019_1592_TRD", # CM009
  #   "FE002_16_TRD_2013-07-14_16-53-45", # FE001
  #   "FE003_17_TRD",  # FE002
  #   "CM002_124_TRD",  # CM001
  #   "BS016_1084_LOG"  # BS017
  # )),

  tar_target(centerFolder, get_center_folders(weaningFolder)),
  tar_target(
    weaningsCentersTRD, {
      import_folder(centerFolder, "TRD")
    },
    packages = "furrr",
    format = "qs",
    pattern = centerFolder,
    iteration = "list"
  ),

  tar_target(
    weaningsCentersLOG, {
      import_folder(centerFolder, "LOG")
    },
    packages = "furrr",
    format = "qs",
    pattern = centerFolder,
    iteration = "list"
  ),


  tar_target(
    weaningsTRD, {
      weaningsCentersTRD |>
        dplyr::bind_rows() |>
        fix_wrong_hours() |>
        dplyr::filter(!.data[["id_univoco"]] %in% patientsToRemove)
    # import_folders(
    #   weaningFolder, "TRD",
    #   patients2remove = patientsToRemove,
    #   file2skip = fileToSkip
    # )
    },
    # packages = "furrr",
    format = "qs"
  ),

  tar_target(problematicDupes, get_problematic_dupes(weaningsTRD)),

  tar_target(
    weaningsLOG,{
      weaningsCentersLOG |>
        dplyr::bind_rows() |>
        fix_wrong_hours() |>
        dplyr::filter(!.data[["id_univoco"]] %in% patientsToRemove)
    # import_folders(
    #   weaningFolder, "LOG",
    #   patients2remove = patientsToRemove,
    #   file2skip = fileToSkip
    # )
    },
    # packages = "furrr",
    format = "qs"
  ),





# transform data --------------------------------------------------


  tar_target(
    pt_names,
    import_patients(patient_to_remove = patientsToRemove),
    format = "qs"
  ),
  tar_target(pt_ids, get_id(pt_names)),
  tar_target(pt_registry, import_registry(), format = "qs"),


# AP substets for plots and report [can we remove them?!] ---------

  tar_target(weaning_subset, extract_weaning_subset(pt_registry)),

  tar_target(
    weaning_log_subset,
    extract_weaning_log_subset(weaningsLOG, weaning_subset)
  ),

  tar_target(
    weaning_log_filtered,
    extract_weaning_log_filtered(weaning_log_subset)
  ),

  tar_target(
    weaning_trd_subset,
    extract_weaning_trd_subset(weaningsTRD, weaning_subset)
  ),




  # plots -----------------------------------------------------------
  tar_target(ggPatPerCentro, {
    weaningsTRD |>
      distinct(id_univoco) |>
      mutate(folder = stringr::str_sub_all(id_univoco, 1, 2)) |>
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
      filter(id_univoco == "BS008") |>
      filter(date == "2013-11-27") |>
      select_if( function(x) {!all(is.na(x))} ) |>
      gather(-ora, key = "var", value = "value") |>
      ggplot(aes(x = ora,
                 y = value)) +
      geom_point(size = 0.5) +
      facet_wrap(~ var, scales = "free")
  }),

  tar_target(ggWeanVariablesSel, {
    weaningsTRD |>
      filter(id_univoco == "BS008") |>
      select(date,
             ora,
             lavoro_respiratorio_del_ventilatore_joule_l,
             lavoro_respiratorio_del_paziente_joule_l,
             pressione_di_fine_esp_cm_h2o,
             press_media_vie_aeree_cm_h2o) |>
      pivot_longer(cols = 3:6) |>
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
    patient_history_plot(
      weaningsTRD, weaningsLOG, pt_names, pt_registry, "TS", 15
    )
  }),
  tar_target(patientHistoryPlotTS012, {
    patient_history_plot(
      weaningsTRD, weaningsLOG, pt_names, pt_registry, "TS", 12
    )
  }),
  tar_target(patientHistoryPlotBS002, {
    patient_history_plot(
      weaningsTRD, weaningsLOG, pt_names, pt_registry, "BS", 2
    )
  }),
  tar_target(patientHistoryPlotNO004, {
    patient_history_plot(
      weaningsTRD, weaningsLOG, pt_names, pt_registry, "NO", 04
    )
  }),

  tar_target(ggTentativiPerPaziente, {
    pt_registry |>
      dplyr::select(id_univoco, esito) |>
      dplyr::group_by(esito, id_univoco) |>
      dplyr::tally() |>
      tidyr::pivot_wider(
        names_from = esito,
        values_from = n,
        values_fill = 0
      ) |>
      ggplot(aes(x = Fallito, y = Successo)) +
      geom_count(aes(colour = ..n.., size = ..n..)) +
      labs( title = "Numero di tentativi per paziente")
  }),

  tar_target(ggWeaningSubsetDay, {
    plot_trd(weaningsTRD, pt_registry, color_files = FALSE)
  }),

  tar_target(ggWeaningSubsetFile, {
    plot_trd(weaningsTRD, pt_registry, color_files = TRUE)
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
    plot_trd(weaningsTRD, weaning_subset, moving_avg = TRUE)
  }),





# keras -----------------------------------------------------------

  tar_target(
    baselineArrays,
    # [pt, var] = [none, 2]
    create_pt_ptnames(pt_names, pt_ids),
    pattern = map(pt_ids),
    iteration = "list"
  ),
  tar_target(
    dailyArrays,
    # [pt, days, var] = [none, length(giorno_studio), 3]
    create_pt_weanings(pt_registry, pt_ids),
    pattern = map(pt_ids),
    iteration = "list"
  ),
  tar_target(
    trdArrays,
    # [pt, minutes, days, var] = [none, 1440, length(giorno_studio), ]
    create_pt_trd(weaningsTRD, pt_ids),
    pattern = map(pt_ids),
    iteration = "list"
  ),
  tar_target(
    outArrays,
    # [pt, days, out] = [none, length(giorno_studio), 1]
    create_pt_output(pt_registry, pt_ids),
    pattern = map(pt_ids),
    iteration = "list"
  ),









# report ----------------------------------------------------------

  # compile the report
  tar_render(report, here("reports/report.Rmd")),
  tar_quarto(trd_log_csv_exploration, here("reports/trd_log_csv_exploration.qmd")),




# objects to share -------------------------------------------------
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

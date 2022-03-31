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
tar_option_set()

# End this file with a list of target objects.
list(
  tar_target(
    weaningFolder,
    get_data_path(),
    format = "file"
  ),
  tar_target(
    weaningsTRD,
    import_trd_folders(weaningFolder),
    packages = "furrr",
    format = "qs"
  ),

  # compile the report
  tar_render(report, here("reports/report.Rmd"))
)

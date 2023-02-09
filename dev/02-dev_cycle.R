
# Project packages (TO BE UPDATED EVERY NEW PACKAGE USED) ----------

{
  prj_pkgs <- c(
    "abind", "forcats", "fs", "ggplot2", "ggrepel", "glue", "purrr",
    "readr", "readxl", "scales", "stringr", "tidyquant", "tidyr"
  )
  dev_pkg <- c(
    "av", "magick", "quarto", "targets", "tarchetypes", "withr",
    "quarto", "reticulate", "tensorflow", "keras"
  ) # "littler")
  gh_prj_pkgs <- c("andrie/deepviz")
  meta_pkgs <- c()

  renv::install(c(prj_pkgs, dev_pkg, meta_pkgs))
  devtools::install_github(
    gh_prj_pkgs,
    auth_token = gitcreds::gitcreds_get()$password
  )

  purrr::walk(prj_pkgs, usethis::use_package)

  purrr::walk(c(dev_pkg), ~{
    usethis::use_package(package_name, type = "Suggests")
  })

  purrr::walk(c(gh_prj_pkgs), ~{
    package_name <- stringr::str_extract(.x, "[\\w\\.]+$")
    usethis::use_dev_package(package_name, remote = .x, type = "Suggests")
  })

  usethis::use_tidy_description()
  devtools::document()
}
renv::status()
# renv::snapshot()


# Functions definitions -------------------------------------------

## if you need more structure respect to include your functions inside
## `R/functions.R`, you can create other couple of test/function-script
## by running the following lines of code as needed.

"create_pt_arrays" |>
  usethis::use_test() |>
  basename() |>
  stringr::str_remove("test-") |>
  usethis::use_r()



# Execute ---------------------------------------------------------
rstudioapi::navigateToFile(here::here("dev/03-run_cycle.R"))

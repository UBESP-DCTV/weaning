usethis::use_description()
usethis::use_roxygen_md()
usethis::use_testthat()
usethis::use_tidy_eval()
usethis::use_mit_license()
usethis::use_package_doc()
usethis::use_spell_check()
spelling::spell_check_package()
spelling::update_wordlist()


# add library(checkmate) to testthat/testthat.R
usethis::edit_file(here::here("tests/testthat/setup.R"))

dev_pkg <- c(
  "testthat", "devtools", "fs", "spelling", "distill", "visNetwork",
  "webshot", "qs", "bs4Dash", "gt", "markdown", "pingr", "shiny",
  "shinybusy", "shinyWidgets"
)
renv::install(dev_pkg)
purrr::walk(dev_pkg, usethis::use_package, type = "Suggests")

prj_pkg <- c(
  "stringr", "readr", "lubridate", "checkmate", "usethis",
  "unheadr", "janitor", "dplyr", "purrr", "furrr", "here", "DT"
)
renv::install(prj_pkg)
purrr::walk(prj_pkg, usethis::use_package)


gh_pkgs <- c(
  "ropensci/targets",
  "ropensci/tarchetypes"
)
renv::install(gh_pkgs)
purrr::walk(gh_pkgs, ~{
  package_name <- stringr::str_extract(.x, "[\\w\\.]+$")
  usethis::use_dev_package(package_name, remote = .x)
})


usethis::use_tidy_description()

renv::status()


usethis::edit_r_environ("project")
usethis::use_r("utils")

usethis::use_r("import_trd")
usethis::use_test("import_trd")

usethis::use_data_raw()

fs::file_move(
  here::here("weaning-imports.R"),
  here::here("dev/old/weaning-imports.R")
)

targets::tar_script()
targets::tar_renv(extras = character(0))

usethis::use_r("get_problematic_dupes")

usethis::use_test("import_log")
usethis::use_r("import_log")

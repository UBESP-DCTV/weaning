source("renv/activate.R")

options(
  warnPartialMatchDollar = TRUE,
  warnPartialMatchArgs   = TRUE,
  warnPartialMatchAttr   = TRUE
)

options(tidyverse.quiet = TRUE)

stopifnot(
  `env var "PROJ_TITLE" must be set` = Sys.getenv("PROJ_TITLE") != "",
  `env var "PROJ_URL" must be set` = Sys.getenv("PROJ_URL") != ""
)

if (interactive()) {#
  suppressPackageStartupMessages(suppressWarnings({
    library(devtools)
    library(usethis)
    library(testthat)
    library(checkmate)
  }))
}


library(targets)

.get_prj_shared_path <- function() {
  prj_shared_path <- Sys.getenv("PRJ_SHARED_PATH")
  if (prj_shared_path == "") {
    usethis::ui_stop("
      Environmental variable {usethis::ui_field('PRJ_SHARED_PATH')} is not set.
      You must set it to the shared path for the project.
      You can set it in the {usethis::ui_value('.Renviron')} file.
      You can open the project {usethis::ui_value('.Renviron')} file calling {usethis::ui_code('usethis::edit_r_environ(\"project\")')}.
    ")
  }

  if (!(fs::is_dir(prj_shared_path))) {
    usethis::ui_stop("
      Environmental variable {usethis::ui_field('PRJ_SHARED_PATH')} is set to {usethis::ui_value(prj_shared_path)}.
      That path is not a valid folder.
      You must provide a valid folder in {usethis::ui_field('PRJ_SHARED_PATH')}.
    ")
  }

  prj_shared_path
}

tar_config_set(
  store = file.path(.get_prj_shared_path(), "_targets")
)

tar_config_set(
  script = here::here("_targets.R"),
  store = file.path(.get_prj_shared_path(), "_targets"),
  config = "tests/testthat/_targets.yaml"
)

tar_config_set(
  script = here::here("_targets.R"),
  store = file.path(.get_prj_shared_path(), "_targets"),
  config = "reports/_targets.yaml"
)

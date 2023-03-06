library(reticulate)
usethis::use_package("reticulate")

path_to_python <- install_python()
virtualenv_create("r-reticulate", python = path_to_python)


# Restart ---------------------------------------------------------
library(tensorflow)
usethis::use_package("tensorflow")
install_tensorflow(version = "cpu", envname = "r-reticulate")

# Restart ---------------------------------------------------------
library(keras)
usethis::use_package("keras")
install_keras(envname = "r-reticulate")


# Restart ---------------------------------------------------------
library(reticulate)
library(tensorflow)
library(keras)
tf$constant("Hello Tensorflow!")


# aux -------------------------------------------------------------
renv::use_python()

install.packages("deepviz")
usethis::use_package("deepviz", type = "Suggests")

# pkgs for model plot
reticulate::virtualenv_install(packages = "pydot")
reticulate::py_install(
  "pydot",
  envname = "r-reticulate",
  pip = TRUE,
  method = "virtualenv"
)
reticulate::conda_install(
  envname = "r-reticulate",
  packages = "graphviz"
)



# model definition script -----------------------------------------
use_r("define_keras_model")

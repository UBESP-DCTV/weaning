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
install.packages("deepviz")
usethis::use_package("deepviz", type = "Suggests")

use_r("define_keras_model")

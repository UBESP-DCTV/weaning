# The idea behind this script is to provide a template for a simple,
# small, self-contained, and sharable project, developed with high level
# of robustness and credibility.
#
#
# Basic workflow
# ==================
#
# 1. Define a problem (writing the corresponding tests)
#
# 2. Develop a solution (writing the corresponding functions)
#
# 3. Use it for your aims (doing/executing your analyses)
#
#
# Advantages
# ==========
#
# 1. You can share a single script containing all your project's
# analyses and tests
#
# 2. Extremely easy to develop with near-zero infrastructure or
# requirements
#
# 3. You can continuously developing and testing your code by simply
# restarting R (CTRL/CMD + SHIFT + F10 in RStudio), and selecting and
# running everything (CTRL/CMD + A + CTRL/CMD + RETURN in RStudio)
#
#
# Disadvantage
# ============
#
# 1. not suitable for high structured and complex project (in those
# cases, you can see https://github.com/UBESP-DCTV/laims.analysis)
#
#
# Info
# ====
#
# For comments/suggestion, please write me:
#
# - GitHub/Telegram/Twitter: @CorradoLanera
# - mail: Corrado [dot] Lanera [at] gmail.com
#
# License: [WTFPL](http://www.wtfpl.net/about/)
#
library(tidyverse)
library(here)


# Functions -------------------------------------------------------

my_sum <- function(...) {
  sum(...)
}








# Analyses/Executions ---------------------------------------------

my_sum(1, 2)
my_sum(1, 2, NA)
my_sum(1, 2, NA, na.rm = TRUE)








# tests -----------------------------------------------------------

library(testthat)
library(checkmate)

# with_reporter() enclosse in an interactive session all the testing
# environment and machinery of {testthat}.
# You can use default_reporter() for a general summary of your
# tests, or you can use check_reporter() for a detailed reporter
# that highlight error also
with_reporter(default_reporter(), {

  # with context() you can define general test headers
  context("Sums")

  # test_that() enclose (sets) of supposed single-purpose tests
  test_that("my_sum works", {
    # setup
    one <- 1
    two <- 2

    # evaluation
    result <- my_sum(one, two)
    result_na <- my_sum(one, two, NA)
    result_narm <- my_sum(one, two, NA, na.rm = TRUE)

    # tests
    expected <- 3
    result |> expect_equal(expected)
    result_na |> expect_equal(NA_real_)
    result_narm |> expect_equal(expected)
  })


})

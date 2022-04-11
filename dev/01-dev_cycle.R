# Update documentation and namespaces -----------------------------

usethis::use_tidy_description()
# CTRL + SHIFT + D


# Update project packages dependencies  ---------------------------
renv::status()
# renv::snapshot()


# Run development tests -------------------------------------------
# CTRL + SHIFT + T


# Check spelling --------------------------------------------------
spelling::spell_check_package()
# spelling::update_wordlist()
# CTRL + SHIFT + D

# Check full CRAN tests
# CTRL + SHIFT + E

# Run the pipeline
source(here::here("run.R"))

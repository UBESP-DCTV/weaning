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
# CTRL + SHIFT + D
# spelling::update_wordlist()

# Check full CRAN tests
# CTRL + SHIFT + E

# Run teh pipeline
source(here::here("run.R"))

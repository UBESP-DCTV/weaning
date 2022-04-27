library(tidyverse)
library(targets)

a <- tar_read(problematicDupes)

a$ora |> class()

glimpse(a)

fix_wrong_hours(a)

b <- tar_read(weaningsTRD)

c <- fix_wrong_hours(b)


get_problematic_dupes(c)

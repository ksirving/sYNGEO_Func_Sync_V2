###########################################################################
#                            Track change in the repo                             #
###########################################################################

library(here)
library(usethis)

riv_dir <- "~/Documents/post-these/isu/RivFishTimeBiodiversityFacets/_targets/objects/" 
list.files(riv_dir)
temperature_data <- c("at_mv_avg", "wt_mv_avg")
file.copy(
  from = paste0(riv_dir, temperature_data),
  to = here("input_data", "Env"),
  overwrite = TRUE 
)

read.table(file(here("input_data", "Env", "at_mv_avg")))

# Add loc
riv_dir <- "~/Documents/post-these/isu/RivFishTimeBiodiversityFacets/data/"
list.files(riv_dir)
site_files <- c("site_desc_loc.rda")
file.copy(from = paste0(riv_dir, site_files), to = here("input_data", "Env"))
# Good:

file.copy(
  from = here("doc", "b-temperature.Rmd"),
  to = here("doc", "a-match-rivfishtime-funcsync.Rmd")
)

use_readme_rmd()

file.copy(
  from = paste0(riv_dir, "formated_wt"),
  to = here("input_data", "Env"),
  overwrite = TRUE
)
load(here("input_data", "Env", "formated_wt"))

riv_data_dir <- "~/Documents/post-these/isu/RivFishTimeBiodiversityFacets/data/" 
file.copy(
  from = paste0(riv_data_dir, c("awt.csv", "filtered_water_temperature.csv", "no_filtered_water_temperature.csv")),
  to = here("input_data", "Env"),
  overwrite = TRUE
)


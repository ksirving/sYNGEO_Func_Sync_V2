###########################################################################
#                            Track change in the repo                             #
###########################################################################

library(here)

riv_dir <- "~/Documents/post-these/isu/RivFishTimeBiodiversityFacets/_targets/objects/" 
list.files(riv_dir)
temperature_data <- c("at_mv_avg", "wt_mv_avg")
file.copy(from = paste0(riv_dir, temperature_data), to = here("input_data", "Env"))

read.table(file(here("input_data", "Env", "at_mv_avg")))

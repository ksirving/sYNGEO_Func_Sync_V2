## connectivity leave one out
library(tidyverse)
library(tidyr)

## interpolated single traits
syncDF <- read.csv("output_data/sync/03_sync_data_funcgroup_traitgroup_similarity_euclidean_dist_interpolated.csv")

load(file= "output_data/sync/02_Oceania_single_traits_interpolated_site_sync_one_out.RData")
oc_sync <- synchrony_axis

load(file= "output_data/sync/02_USA_single_traits_interpolated_site_sync_one_out.RData")
usa_sync <- synchrony_axis

load(file= "output_data/sync/02_Europe_single_traits_interpolated_site_sync_one_out.RData")
eu_sync <- synchrony_axis


interDF <- rbind(oc_sync, usa_sync, eu_sync)

unique(interDF$TraitGroup) 
unique(oc_sync$TraitGroup) 
unique(usa_sync$TraitGroup) 
unique(eu_sync$TraitGroup) 

unique(syncDF$TraitGroup) 
head(syncDF) 
head(interDF) # 

tail(interDF$X)
unique(interDF$Trait)
unique(syncDF$Trait)

syncDF <- syncDF %>%
  dplyr::select(Region:TraitGroup, Pair, Connectivity:MaxDist)

interDF <- interDF %>%
  # select(-X) %>%
  mutate(Pair = paste(Site_ID1, ".", Site_ID2, sep="")) 

length(unique(syncDF$Pair)) # 206535
length(unique(interDF$Pair)) # 207299 - fix this

sunctest <- syncDF %>% filter(Pair == "S7991.S7990")

intertest <- interDF %>% filter(Pair == "S7991.S7990")

intertest
sunctest

# ?left_join
interDF_join <- left_join(interDF, syncDF, by=c("Region", "Trait", "TraitGroup", "Pair"))
head(interDF_join)
sum(is.na(interDF_join))

write.csv(interDF_join, "output_data/sync/03_sync_data_funcgroup_traitgroup_similarity_euclidean_dist_interpolated_LOO.csv")


#### interpolated ordination 
syncDF <- read.csv("output_data/sync/03_sync_data_funcgroup_traitgroup_similarity_euclidean_dist_interpolated_ordination.csv")
head(syncDF)

load(file= "output_data/sync/02_Oceania_ordination_interpolated_site_sync_one_out.RData")
oc_sync <- synchrony_axis

load(file= "output_data/sync/02_USA_ordination_interpolated_site_sync_one_out.RData")
usa_sync <- synchrony_axis

load(file= "output_data/sync/02_Europe_ordination_interpolated_site_sync_one_out.RData")
eu_sync <- synchrony_axis
head(synchrony_axis)
unique(synchrony_axis$TraitGroup)

interDF <- rbind(oc_sync, usa_sync, eu_sync)

head(syncDF)
head(interDF)

syncDF <- syncDF %>%
  dplyr::select(-Country.x) %>% 
  rename(Country = Country.y) %>%
  select(Region:Country, TraitGroup, Pair, Connectivity:MaxDist)

interDF <- interDF %>%
  # select(-X) %>%
  mutate(Pair = paste(Site_ID1, ".", Site_ID2, sep="")) 

length(unique(syncDF$Pair)) # 207299
length(unique(interDF$Pair)) # 207299 

unique(interDF$TraitGroup) ## only 3, hasn't worked

interDF_join <- left_join(interDF, syncDF, by=c("Region","Axis", "TraitGroup", "Pair"))

sum(is.na(interDF))
dim(interDF_join)
interDF_join <- distinct(interDF_join)

write.csv(interDF_join, "output_data/sync/03_sync_data_funcgroup_traitgroup_similarity_euclidean_dist_interpolated_ordination_LOO.csv")

# Calculate differences in synchrony --------------------------------------



# climate & flow ----------------------------------------------------------

## flow

syncDF <- read.csv("output_data/sync/03_sync_data_funcgroup_traitgroup_similarity_euclidean_dist_interpolated.csv")

load(file= "output_data/sync/02_Oceania_flow_between_all_sites_biogeographic_regions_interpolated_sync_one_out.RData")
oc_sync <- synchrony_axis

load(file= "output_data/sync/02_USA_flow_between_all_sites_biogeographic_regions_interpolated_sync_one_out.RData")
usa_sync <- synchrony_axis

load(file= "output_data/sync/02_Europe_flow_between_all_sites_biogeographic_regions_interpolated_sync_one_out.RData")
eu_sync <- synchrony_axis


interDF <- rbind(oc_sync, usa_sync, eu_sync)

head(syncDF) 
head(interDF) # 

tail(interDF$X)
unique(interDF$Trait)
unique(syncDF$Trait)

syncDF <- syncDF %>%
  dplyr::select(Region:TraitGroup, Pair, Connectivity:MaxDist)

interDF <- interDF %>%
  # select(-X) %>%
  mutate(Pair = paste(Site_ID1, ".", Site_ID2, sep="")) 

length(unique(syncDF$Pair)) # 207299
length(unique(interDF$Pair)) # 204757 - fix this

sunctest <- syncDF %>% filter(Pair == "S7991.S7990")

intertest <- interDF %>% filter(Pair == "S7991.S7990")

intertest
sunctest

# ?left_join
interDF_join <- left_join(interDF, syncDF, by=c("Region",  "Pair"))
interDF_join <- interDF_join %>%
  dplyr::select(-Trait, -TraitGroup) %>%
  distinct()

head(interDF_join)
sum(is.na(interDF_join))

write.csv(interDF_join, "output_data/sync/03_sync_data_funcgroup_flow_similarity_euclidean_dist_interpolated_LOO.csv")

## temperature

## interpolated single traits
syncDF <- read.csv("output_data/sync/03_sync_data_funcgroup_traitgroup_similarity_euclidean_dist_interpolated.csv")

load(file= "output_data/sync/02_Oceania_temperature_between_all_sites_biogeographic_regions_interpolated_sync_one_out.RData")
oc_sync <- synchrony_axis

load(file= "output_data/sync/02_USA_temperature_between_all_sites_biogeographic_regions_interpolated_sync_one_out.RData")
usa_sync <- synchrony_axis

load(file= "output_data/sync/02_Europe_temperature_between_all_sites_biogeographic_regions_interpolated_sync_one_out.RData")
eu_sync <- synchrony_axis


interDF <- rbind(oc_sync, usa_sync, eu_sync)

head(syncDF) 
head(interDF) # 

tail(interDF$Pair)
unique(interDF$Trait)
unique(syncDF$Trait)

syncDF <- syncDF %>%
  dplyr::select(Region:TraitGroup, Pair, Connectivity:MaxDist)

interDF <- interDF %>%
  # select(-X) %>%
  mutate(Pair = paste(Site_ID1, ".", Site_ID2, sep="")) 

tail(interDF$Pair)
tail(syncDF$Pair)

length(unique(syncDF$Pair)) # 207299
length(unique(interDF$Pair)) # 207299 

sunctest <- syncDF %>% filter(Pair == "S7991.S7990")

intertest <- interDF %>% filter(Pair == "S7991.S7990")

intertest
sunctest

# ?left_join
interDF_join <- left_join(interDF, syncDF, by=c("Region",  "Pair"))
interDF_join <- interDF_join %>%
  dplyr::select(-Trait, -TraitGroup) %>%
  distinct()

head(interDF_join)
sum(is.na(interDF_join))

write.csv(interDF_join, "output_data/sync/03_sync_data_funcgroup_temp_similarity_euclidean_dist_interpolated_LOO.csv")


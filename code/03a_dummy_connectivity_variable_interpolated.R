## add dummy variabkle within/between basins

library(sp)
library(raster)
library(ggplot2)
library(dplyr)
library(tidyverse)

getwd()

## upload fish abundance and site data
originaldata <- read.csv("input_data/Bio/fishdata_selection_basins_same_time_window_10262020.csv")
head(originaldata)

## take only sites and basins
sites <- originaldata %>%
  dplyr::select(SiteID, HydroBasin) %>%
  distinct()

## all site synchrony

sync <- read.csv("output_data/sync/02_funcgroup_traitgroup_between_all_sites_single_traits_biogeographic_regions_interpolated.csv")
head(sync)

## join first site basin
site1 <- left_join(sync, sites, by = c("Site_ID1" = "SiteID"))
head(site1)

## change names so we know it's for site 1
site1 <- site1 %>%
  rename(HydroBasin1 = HydroBasin, Pair = X)

## join site 2

site2 <- left_join(site1, sites, by = c("Site_ID2" = "SiteID"))
head(site2)

## change names so we know it's for site 2
site2 <- site2 %>%
  rename(HydroBasin2 = HydroBasin)

head(site2)


## create dummy variable. If hydrobasin 1 matches hydrobasin 2 = within site (1), if not then between sites (0)

con <- site2 %>%
  mutate(Connectivity = ifelse(HydroBasin1 == HydroBasin2, 1, 0))

head(con)

write.csv(con, "output_data/sync/03_all_sync_by_groups_bioreg_connectivity_interpolated.csv")

rm(con)
# calculate distance ------------------------------------------------------


sync <- read.csv("output_data/sync/03_all_sync_by_groups_bioreg_connectivity_interpolated.csv")
head(sync)

## define pairs
syncsites <- sync %>%
  dplyr::select(-X, -Pair, -Trait, -Correlation, -Region, -Country) %>%
  filter(TraitGroup == "FoodAquisition") %>%
  # tidyr::pivot_wider(names_from = Trait, values_from = Correlation) %>%
  mutate(Pair = paste(Site_ID1, ".", Site_ID2, sep="")) %>%
  mutate(Euclid_Dist_Meters = 0, Similarity = 0, MeanLat = 0, MeanLon = 0) %>%
  # dplyr::select(-FeedingGroup, ) %>%
  distinct()

head(syncsites)

pairs <- unique(syncsites$Pair)
tail(pairs)
length(pairs) ##  207299

## site coords

fish_ab <- read.csv("input_data/Bio/fishdata_selection_basins_same_time_window_10262020.csv")
head(fish_ab)

## get coords
SiteCoords <- fish_ab %>%
  dplyr::select(SiteID, Latitude, Longitude) %>%
  distinct()

## loop over pairs - takes a long ass time, go do something else for a bit...

for(p in 1:length(pairs)) {

  ## get pair from sync data

  pairx <- syncsites %>%
    filter(Pair == pairs[p])
  # pairx
  ## define sites
  S1 <- pairx$Site_ID1
  S2 <- pairx$Site_ID2

  # S1

  ## get coords for each site
  CoordsS1 <- SiteCoords %>%
    filter(SiteID == S1) %>%
    dplyr::select(Longitude, Latitude, SiteID)

  CoordsS2 <- SiteCoords %>%
    filter(SiteID == S2) %>%
    dplyr::select(Longitude, Latitude, SiteID)

  sp::coordinates(CoordsS1) <- c("Longitude", "Latitude")
  sp::coordinates(CoordsS2) <- c("Longitude", "Latitude")

  #Make a distance matrix
  dst <- pointDistance(CoordsS1,CoordsS2, lonlat=TRUE)
  # str(dst)
  # get mean latitude/longitude
  MeanLat <- (CoordsS1$Latitude+CoordsS2$Latitude)/2
  MeanLon <- (CoordsS1$Longitude+CoordsS2$Longitude)/2

  ## add to dataframe
  syncsites[p,8] <- dst
  syncsites[p,10] <- MeanLat
  syncsites[p,11] <- MeanLon
 head(syncsites)

}

save(syncsites, file = "output_data/sync/03_all_pair_distances.RData")



# Combine with single trait DF --------------------------------------------

sync <- read.csv("output_data/sync/02_funcgroup_traitgroup_between_all_sites_single_traits_biogeographic_regions_interpolated.csv")
head(sync)

load(file = "output_data/sync/03_all_pair_distances.RData") ## syncsites

## make Pair column
sync <- sync %>%
  unite(Pair, Site_ID1:Site_ID2,sep = ".", remove=F)

## take only distance columns
sync_sub <- syncsites %>%
  dplyr::select(Connectivity:MeanLon)

## join
all_sync <- left_join(sync, sync_sub, by = "Pair")
head(all_sync)
## convert to similarities

syncDF <- all_sync %>%
  group_by(TraitGroup, Region, Trait) %>%
  mutate(MaxDist = max(Euclid_Dist_Meters)) %>%
  mutate(Similarity = 1-(Euclid_Dist_Meters/MaxDist))

head(syncDF)


save(syncDF, file = "output_data/sync/03_sync_data_funcgroup_traitgroup_similarity_euclidean_dist_interpolated.RData")


# Join to ordination DF -------------------------------------------------

## upload 
ordDF <- read.csv( "output_data/sync/02_funcgroup_traitgroup_between_all_sites_ordination_biogeographic_regions_interpolated.csv")
head(ordDF)

## take only distance columns
sync_sub <- syncsites %>%
  dplyr::select(Connectivity:MeanLon)

## remove X and make Pair column
ordDF <- ordDF %>%
 dplyr::select(-X) %>%
  mutate(Pair = paste(Site_ID1, ".", Site_ID2, sep="")) 
  
## join
all_sync <- left_join(ordDF, sync_sub, by = "Pair")
head(all_sync)
## convert to similarities

syncDF <- all_sync %>%
  group_by(TraitGroup, Region, Axis) %>%
  mutate(MaxDist = max(Euclid_Dist_Meters)) %>%
  mutate(Similarity = 1-(Euclid_Dist_Meters/MaxDist))

head(syncDF)

save(syncDF, file = "output_data/sync/03_sync_data_ordination_traitgroup_similarity_euclidean_dist_interpolated.RData") 


# Combine with single traits LOO ------------------------------------------

load(file= "output_data/sync/02_Oceania_single_traits_interpolated_site_sync_one_out.RData")
oc_sync <- synchrony_axis

load(file= "output_data/sync/02_USA_single_traits_interpolated_site_sync_one_out.RData")
usa_sync <- synchrony_axis

load(file= "output_data/sync/02_Europe_single_traits_interpolated_site_sync_one_out.RData")
eu_sync <- synchrony_axis

interDF <- rbind(oc_sync, usa_sync, eu_sync)

## take only distance columns
sync_sub <- syncsites %>%
  dplyr::select(Connectivity:MeanLon)

## remove X and make Pair column
interDF <- interDF %>%
  # dplyr::select(-X) %>%
  mutate(Pair = paste(Site_ID1, ".", Site_ID2, sep="")) 

## join
all_sync <- left_join(interDF, sync_sub, by = "Pair")
head(all_sync)
tail(all_sync)

## convert to similarities

syncDF <- all_sync %>%
  group_by(TraitGroup, Region, Trait) %>%
  mutate(MaxDist = max(Euclid_Dist_Meters)) %>%
  mutate(Similarity = 1-(Euclid_Dist_Meters/MaxDist))

head(syncDF)


save(syncDF, file = "output_data/sync/03_sync_data_funcgroup_traitgroup_similarity_euclidean_dist_interpolated_LOO.RData")


# Combine with ordination LOO ---------------------------------------------

load(file= "output_data/sync/02_Oceania_ordination_interpolated_site_sync_one_out.RData")
oc_sync <- synchrony_axis

load(file= "output_data/sync/02_USA_ordination_interpolated_site_sync_one_out.RData")
usa_sync <- synchrony_axis

load(file= "output_data/sync/02_Europe_ordination_interpolated_site_sync_one_out.RData")
eu_sync <- synchrony_axis

interDF <- rbind(oc_sync, usa_sync, eu_sync)

## take only distance columns
sync_sub <- syncsites %>%
  dplyr::select(Connectivity:MeanLon)

## remove X and make Pair column
interDF <- interDF %>%
  # dplyr::select(-X) %>%
  mutate(Pair = paste(Site_ID1, ".", Site_ID2, sep="")) 

## join
all_sync <- left_join(interDF, sync_sub, by = "Pair")
head(all_sync)
tail(all_sync)

## convert to similarities

syncDF <- all_sync %>%
  group_by(TraitGroup, Region, Axis) %>%
  mutate(MaxDist = max(Euclid_Dist_Meters)) %>%
  mutate(Similarity = 1-(Euclid_Dist_Meters/MaxDist))

head(syncDF)

save(syncDF, file = "output_data/sync/03_sync_data_funcgroup_traitgroup_similarity_euclidean_dist_interpolated_ordination_LOO.RData")


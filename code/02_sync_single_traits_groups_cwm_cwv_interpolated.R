### synchrony on single traits

## packages
library(tidyverse)
library(reshape2)
library(tidyr)
library(dplyr)
library(synchrony)
library(codyn)
# library(rfishbase)
library(munfold)
library(data.table)
library(gdata)
library(here)

getwd()
## upload fish abundance and site data
originaldata <- read.csv(here("input_data/Bio/fishdata_selection_basins_same_time_window_10262020.csv"))
head(originaldata)

# test <- originaldata %>% filter(SiteID == "S6654")
# test

## upload and format community weighted mean traits - all groups

trait_matrix <- read.csv(here("output_data/01_trt_single_traits_interpolated_cwm_cmv.csv"))
head(trait_matrix)
## combine all groups

all_groups <- trait_matrix %>%
  mutate(BiogeoRegion = case_when(Country %in% c("FIN", "SWE", "GBR", "ESP", "FRA") ~ "Europe",
                                  Country== "AUS" ~ "Oceania", Country == "USA" ~ "USA"))

source("https://raw.githubusercontent.com/alaindanet/fishcom/master/R/synchrony.R")

sync_mat <- matrix(
  c(0, 0, 1, 1),
  nrow = 2,
  byrow = TRUE,
  dimnames = list(
    paste0("t", c(1, 2)),
    c("sp1", "sp2")
  )
)

# Complete synchrony
sync_mat
str(sync_mat)
class(sync_mat)
compute_synchrony(cov(sync_mat))
?sync_mat
?
# Complete asynchrony
async_mat <- sync_mat
async_mat[, 1] <- rev(async_mat[, 1])
compute_synchrony(cov(async_mat))
```

# between all sites - within biogeogrpahic region -----------------------

## define biogeogrpahic region

head(all_groups)

regionsID<-unique(all_groups$BiogeoRegion) # 3 regions
synchronyx = NULL
region = 3


  ### loop over regions
  for (region in 1:length(regionsID)) {
    
    basindata<-all_groups[all_groups$BiogeoRegion==regionsID[region],]
    head(basindata)
    basindata <- basindata[order(basindata$SiteID),]
    
    ### loop over axis
    Ntraits<-unique(basindata$Trait)
    Ntraits
    
    for (ax in 1: length(Ntraits)) {
      
     
      
      trait_data<-basindata[basindata$Trait==unique(basindata$Trait)[ax],]
      # sum(is.na(trait_data))
      head(trait_data)
      
      # make df wide - mean
      trait_CWM  <- trait_data %>% 
        dplyr::select(-c(X, site_year, Country, HydroBasin, CWV, Latitude, Longitude) ) %>%
        spread(SiteID, CWM) #%>% ## some NAs appear here, don't have all trait scores for all site years
      
      # make df wide - variance
      trait_CWV  <- trait_data %>% 
        dplyr::select(-c(X, site_year, Country, HydroBasin, CWM, Latitude, Longitude) ) %>%
        spread(SiteID, CWV) #%>% ## some NAs appear here, don't have all trait scores for all site years
      
      # flip data
      trait_CWM <- (trait_CWM)[,-c(1:3)]
      trait_CWV <- (trait_CWV)[,-c(1:3)]
      
      ### synchrony 
      cc <- expand.grid(colnames(trait_CWM), colnames(trait_CWM), KEEP.OUT.ATTRS = FALSE)
      
      synchrony <- sapply(seq_len(nrow(cc)), function(k) {
        i <- cc[k,1]
        j <- cc[k,2]
 
        sync_mat <- matrix(
          c(trait_CWM[, i],trait_CWM[,j]),
          nrow = 10,
          byrow = F,
          dimnames = list(years,
            c("site1", "site2")
          )
        )
        
        compute_synchrony(sync_mat)
      })
      
     
      
      ### functional diversity: Temporal average of Community Weighted Variance
      
      cc <- expand.grid(colnames(trait_CWV), colnames(trait_CWV), KEEP.OUT.ATTRS = FALSE)
      
      diversity <- sapply(seq_len(nrow(cc)), function(k) {
        i <- cc[k,1]
        j <- cc[k,2]
        
        div_mat <- matrix(
          c(trait_CWV[, i],trait_CWV[,j]),
          nrow = 10,
          byrow = F,
          dimnames = list(years,
                          c("site1", "site2")
          )
        )
        
        mean(div_mat)
        
      })
      
      # diversity
      ### functional distance: Difference in temporal average of Community Weighted Mean
      
      cc <- expand.grid(colnames(trait_CWM), colnames(trait_CWM), KEEP.OUT.ATTRS = FALSE)
      
      distance <- sapply(seq_len(nrow(cc)), function(k) {
        i <- cc[k,1]
        j <- cc[k,2]
        
        dist_mat <- matrix(
          c(trait_CWM[, i],trait_CWM[,j]),
          nrow = 10,
          byrow = F,
          dimnames = list(years,
                          c("site1", "site2")
          )
        )
        
        mean(1-dist_mat[,1]/dist_mat[,2])
        
      })
      
      
      ## combine all
      synchrony <- cbind(cc, synchrony, distance, diversity)
      
      ## add traits and region
      synchrony <- synchrony %>%
        mutate(Trait = Ntraits[ax], Region = regionsID[region])
      
      ## add tio main DF
      synchronyx <- rbind(synchronyx, synchrony)
      
      
    }
    
    
  }
  
  

synchrony_axis <- synchronyx %>%
  rename(Site_ID1 = Var1, Site_ID2 = Var2) %>%
  mutate(Pair = paste0(Site_ID1, ".", Site_ID2))

head(synchrony_axis)
length(unique(synchrony_axis$Trait))
length(unique(synchrony_axis$Region))


###save results
write.csv(synchrony_axis, "output_data/sync/02_between_all_sites_single_traits_CWM_CWV.csv")



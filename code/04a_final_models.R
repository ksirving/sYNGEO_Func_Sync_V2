### final models

## analysis: functional synchrony ~ distance * connectivity + functional diversity + environmental synchrony

# packages

library(tidyverse)
library(tidylog)
library("easystats")

## directory for figures
out.dir <- "/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/sYNGEO_Func_Sync_V2/Figures/"

## function to normalise data
min_max_norm <- function(x) {
  (x - min(na.omit(x))) / (max(na.omit(x)) - min(na.omit(x)))
}


# upload data -------------------------------------------------------------

## functional synchrony

load(file = "output_data/sync/03_sync_traits_CWM_CWV_distances.RData")
funcsync <- syncDF %>%
  rename(Sync = synchrony) %>%
  select(-X) %>%
  filter(!Site_ID1 == Site_ID2)

head(funcsync)
unique(funcsync$Trait)
## environment synchrony
load(file="output_data/sync/03_sync_temp_distances.RData")
tempsync <- syncDF %>%
  # mutate(SyncType = "TSync")  %>%
  pivot_wider(names_from = Metric, values_from = synchrony) %>%
  filter( !Site_ID1 == Site_ID2)## remove pairs comrised of same sites
head(tempsync)

## remove for now  - fix later if needed
tempsync <- na.omit(tempsync)

load(file="output_data/sync/03_sync_flow_distances.RData")

flowsync <- syncDF %>%
  # mutate(SyncType = "QSync") %>%
  pivot_wider(names_from = Metric, values_from = synchrony) %>%
  filter( !Site_ID1 == Site_ID2) ## remove pairs comrised of same sites

flowsync <- na.omit(flowsync)

head(flowsync)

## join temp and flow together 

names(flowsync)
names(tempsync)

envsync <- full_join(tempsync, flowsync, by = c("Pair","Site_ID1", "Site_ID2", "Region", "Connectivity", "Euclid_Dist_Meters",
                                                "Similarity", "MeanLat", "MeanLon", "MaxDist"))
## missing rows from missing flow sites (i think)

head(envsync)

## NAs where sites are missing - different for flow and temp
# which(is.na(envsync$Metric.x))
# test <- envsync[2465413:2465421, ]

### join functinal synchrony

allsync <- full_join(funcsync, envsync, by = c("Pair","Site_ID1", "Site_ID2", "Region", "Connectivity", "Euclid_Dist_Meters",
                                               "Similarity", "MeanLat", "MeanLon", "MaxDist")) %>%
  mutate(DistKM = Euclid_Dist_Meters/1000) %>%
  filter(Trait == c( "AVG_MXL","Tp_pref" )) %>%
  select(-qmean_raw, - qmax_raw, -qmin_raw) ## add back if needed

head(allsync)

object.size(allsync)

unique(allsync$Trait)

save(allsync, file = "output_data/sync/04a_all_sync_for_figures.RData")

## split into regions
EurSync <- allsync %>% filter(Region == "Europe")
AusSync <- allsync %>% filter(Region == "Oceania")
USASync <- allsync %>% filter(Region == "USA")

## split into traits - max length
sizeSyncEU <- EurSync %>%
  filter(Trait == "AVG_MXL")

# save(sizeSyncEU, file = "output_data/02_europe_body_size_data_for_model_explo.RData")
# load( file = "output_data/02_europe_body_size_data_for_model_explo.RData") ## sizeSyncEU

sizeSyncAU <- AusSync %>%
  filter(Trait == "AVG_MXL")

sizeSyncUS <- USASync %>%
  filter(Trait == "AVG_MXL")

## split into traits - temp preference
TPSyncEU <- EurSync %>%
  filter(Trait == "Tp_pref")


TPSyncAU <- AusSync %>%
  filter(Trait == "Tp_pref")

TPSyncUS <- USASync %>%
  filter(Trait == "Tp_pref")


# Multicolinearality ------------------------------------------------------

### body sixe
modEur <- lm(Sync~(Connectivity+diversity+DistKM+distance)+annual_avg, data = sizeSyncEU)
summary(modEur)
anova(modEur)
check_collinearity(modEur)

modUS <- lm(Sync~(Connectivity+diversity+DistKM+distance)+annual_avg, data = sizeSyncUS)
summary(modUS)
anova(modUS)
check_collinearity(modUS) ## high VIF in diversity and annual temp, moderate VIF in DistKM

modAU <- lm(Sync~(Connectivity+diversity+DistKM+distance)+annual_avg, data = sizeSyncAU)
summary(modAU)
anova(modAU)
check_collinearity(modAU)

## temp preference
modEur <- lm(Sync~(Connectivity+diversity+DistKM+distance)+annual_avg, data = TPSyncEU)
summary(modEur)
anova(modEur)
check_collinearity(modEur)

modUS <- lm(Sync~(Connectivity+diversity+DistKM+distance)+annual_avg, data = TPSyncUS)
summary(modUS)
anova(modUS)
check_collinearity(modUS) ## high VIF in diversity and annual temp, moderate VIF in DistKM

modAU <- lm(Sync~(Connectivity+diversity+DistKM+distance)+annual_avg, data = TPSyncAU)
summary(modAU)
anova(modAU)
check_collinearity(modAU)

# LM: body size----------------------------------------------------------------------


## per region and connectivity?

sizeSyncEULogW <- sizeSyncEULog %>%
  filter(Connectivity == 1)

sizeSyncEULogB <- sizeSyncEULog %>%
  filter(Connectivity == 0)

sizeSyncAULogW <- sizeSyncAULog %>%
  filter(Connectivity == 1)

sizeSyncAULogB <- sizeSyncAULog %>%
  filter(Connectivity == 0)


## interactions with environmental synchrony
mod2W <- lm(fs~(d+dv_Norm+di_Norm)*tav, data = sizeSyncEULogW)
summary(mod2W)
anova(mod2W)

mod2B <- lm(fs~(d+dv_Norm+di_Norm)*tav, data = sizeSyncEULogB)
summary(mod2B)
anova(mod2B)

mod2W <- lm(fs~(d+dv_Norm+di_Norm)*tav, data = sizeSyncAULogW)
summary(mod2W)
anova(mod2W)

mod2B <- lm(fs~(d+dv_Norm+di_Norm)*tav, data = sizeSyncAULogB)
summary(mod2B)
anova(mod2B)

## Europe
sizeSyncEULog <- sizeSyncEU %>%
  mutate(fs = log(Sync+1), 
         d = log(DistKM+1),
         dv = diversity,
         di = log(distance+1),
         tav = annual_avg,
         c = Connectivity)  %>%
  mutate(di_Norm = min_max_norm(di), dv_Norm = min_max_norm(dv)) ## normalise diversity & distance

head(sizeSyncEULog)
sum(is.na(unique(sizeSyncEULog$DistKM)))
sum(sizeSyncEULog$DistKM == 0)

## interactions with environmental synchrony
mod2a <- lm(fs~(d+c+dv_Norm+di_Norm)*tav, data = sizeSyncEULog)

summary(mod2a) ## all significant, except functional diversity

## r2 = 0.16

anova(mod2a) ### all significant, single effects and interactions

## Australia

sizeSyncAULog <- sizeSyncAU %>%
  mutate(fs = log(Sync+1), 
         d = log(DistKM+1),
         dv = diversity,
         di = log(distance+1),
         tav = annual_avg,
         c = Connectivity)  %>%
  mutate(di_Norm = min_max_norm(di), dv_Norm = min_max_norm(dv)) ## normalise diversity & distance

### LM
## interactions with environmental synchrony
mod2b <- lm(fs~(d+c+dv_Norm+di_Norm)*tav, data = sizeSyncAULog)

summary(mod2b) ## nothing significant
# plot(mod2) ## qq plot heavily tailed

## r2 = 0.03

anova(mod2b) ### functional diversity & temp & connectivity (mod sig), no interactions

## USA

sizeSyncUSLog <- sizeSyncUS %>%
  mutate(fs = log(Sync+1), 
         d = log(DistKM+1),
         dv = diversity,
         di = log(distance+1),
         tav = annual_avg,
         c = Connectivity)  %>%
  mutate(di_Norm = min_max_norm(di), dv_Norm = min_max_norm(dv)) ## normalise diversity & distance

### LM
## interactions with environmental synchrony
mod2c <- lm(fs~(d+c+dv_Norm+di_Norm), data = sizeSyncUSLog) ## remove temp due to colinearity

summary(mod2c) ##  functional diversity, distance, connectivity 
# plot(mod2c) ## qq plot heavily tailed

## r2 = 0.29

anova(mod2c) ### functional diversity, distance, connectivity 


# LM: temperature preference ----------------------------------------------

## check fs for skewness

library(e1071)
skewness(na.omit(TPSyncEU$Sync)) ## -0.291 - mod skewed, log transformed not ok
skewness(na.omit(TPSyncAU$Sync)) ## -0.246 - mod skewed, log transformed not ok
skewness(na.omit(TPSyncUS$Sync)) ## -0.198 - mod skewed, log transformed not ok

## Europe
TPSyncEULog <- TPSyncEU %>%
  mutate(fs = log(Sync+1), 
         d = log(DistKM+1),
         dv = diversity,
         di = log(distance+1),
         tav = annual_avg,
         c = Connectivity)  %>%
  mutate(di_Norm = min_max_norm(di), dv_Norm = min_max_norm(dv)) ## normalise diversity & distance

head(TPSyncEULog)
sum(is.na(unique(TPSyncEULog$DistKM)))
sum(TPSyncEULog$DistKM == 0)

## interactions with environmental synchrony
mod3a <- lm(fs~(d+c+dv_Norm+di_Norm)*tav, data = TPSyncEULog)

summary(mod3a) ## all significant, except functional distance, and func dist * temp

## r2 = 0.03

anova(mod3a) ### all significant, except functional distance, and func dist * temp

## Australia

TPSyncAULog <- TPSyncAU %>%
  mutate(fs = log(Sync+1), 
         d = log(DistKM+1),
         dv = diversity,
         di = log(distance+1),
         tav = annual_avg,
         c = Connectivity)  %>%
  mutate(di_Norm = min_max_norm(di), dv_Norm = min_max_norm(dv)) ## normalise diversity & distance

### LM
## interactions with environmental synchrony
mod3b <- lm(fs~(d+c+dv_Norm+di_Norm)*tav, data = TPSyncAULog)

summary(mod3b) ## distance km, temp and d*t interaction
# plot(mod2) ## qq plot heavily tailed

## r2 = 0.006

anova(mod3b) ###  temp single, temp*d interaction

## USA

TPSyncUSLog <- TPSyncUS %>%
  mutate(fs = log(Sync+1), 
         d = log(DistKM+1),
         dv = diversity,
         di = log(distance+1),
         tav = annual_avg,
         c = Connectivity)  %>%
  mutate(di_Norm = min_max_norm(di), dv_Norm = min_max_norm(dv)) ## normalise diversity & distance

### LM
## interactions with environmental synchrony
mod3c <- lm(fs~(d+c+dv_Norm+di_Norm), data = TPSyncUSLog) ## remove temp due to colinearity

summary(mod3c) ##  all significant except functional distance
# plot(mod2c) ## qq plot heavily tailed

## r2 = 0.05

anova(mod3c) ### KM, connectivity, func diversity


# GLM: body size ----------------------------------------------------------

# Europe
moda <- glm(fs~(d+c+di_Norm+dv_Norm)*tav ,data=sizeSyncEULog,family=Gamma(link = "log"))
summary(moda) # -457326 - lowest so far, 
# anova(mod)

with(summary(moda), 1 - deviance/null.deviance) ##  0.1372422

## Australia
modb <- glm(fs~(c+dv_Norm+d+di_Norm)*tav ,data=sizeSyncAULog,family=Gamma(link = "log"))
summary(modb) ##  -6050.9, nothing significant
anova(modb)

with(summary(modb), 1 - deviance/null.deviance) ## 0.02763475

## USA
modc <- glm(fs~(c+dv_Norm+d+di_Norm) ,data=sizeSyncUSLog,family=Gamma(link = "log"))
summary(modc) ##  -6032.9 - connetivity, distance, functional diversity
anova(modc)

with(summary(modc), 1 - deviance/null.deviance) ## 0.2331024



# GLM: Temp pref ----------------------------------------------------------

# Europe
moda <- glm(fs~(d+c+di_Norm+dv_Norm)*tav ,data=TPSyncEULog,family=Gamma(link = "log"))
summary(moda) # 

with(summary(moda), 1 - deviance/null.deviance) ##  0.027

## Australia
modb <- glm(fs~(c+dv_Norm+d+di_Norm)*tav ,data=TPSyncAULog,family=Gamma(link = "log"))
summary(modb) 
anova(modb)

with(summary(modb), 1 - deviance/null.deviance) ## 0.01

## USA
modc <- glm(fs~(c+dv_Norm+d+di_Norm) ,data=TPSyncUSLog,family=Gamma(link = "log"))
summary(modc) ##
anova(modc)

with(summary(modc), 1 - deviance/null.deviance) ## 0.04

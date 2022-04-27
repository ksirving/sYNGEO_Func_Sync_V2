## analysis: functional synchrony ~ distance * connectivity + functional diversity + environmental synchrony

# packages

library(tidyverse)
library(tidylog)

## directory for figures
out.dir <- "/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/sYNGEO_Func_Sync_V2/Figures/"

## data

## functional synchrony

load(file = "output_data/sync/03_sync_traits_CWM_CWV_distances.RData")
funcsync <- syncDF %>%
  rename(Sync = synchrony) %>%
  select(-X)
head(funcsync)

## environment synchrony
load(file="output_data/sync/03_sync_temp_distances.RData")
tempsync <- syncDF %>%
  # mutate(SyncType = "TSync")  %>%
  pivot_wider(names_from = Metric, values_from = synchrony) %>%
  filter(!Euclid_Dist_Meters == 0, !Similarity == 1) ## remove pairs comrised of same sites
head(tempsync)

## remove for now  - fix later if needed
tempsync <- na.omit(tempsync)

load(file="output_data/sync/03_sync_flow_distances.RData")
flowsync <- syncDF %>%
  # mutate(SyncType = "QSync") %>%
  pivot_wider(names_from = Metric, values_from = synchrony) %>%
  filter(!Euclid_Dist_Meters == 0, !Similarity == 1) ## remove pairs comrised of same sites

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
  mutate(DistKM = Euclid_Dist_Meters/1000)

head(allsync)

unique(allsync$Trait)


# Histograms - data exploration -------------------------------------------
library(scales)
head(allsync)

## take only env vars and conncetivity - format for plots
env_data <- allsync %>%
  select(c(Connectivity, annual_avg:DistKM)) %>%
  distinct() %>%
  pivot_longer(annual_avg:DistKM, names_to = "Variable", values_to = "Value") %>%
  mutate(Connectivity = factor(Connectivity, levels = c(1, 0))) %>%
  filter(!Variable %in% c("qmin_raw", "qmax_raw")) %>%
  mutate(Variable = factor(Variable, levels = c("annual_avg", "summer_avg", "qmean_raw", "DistKM"))) 

## define names
supp.labs <- c("Within Basin", "Between Basin")
names(supp.labs) <- c("1","0")

## define labels
temp.labs <- c("Annual Temp", "Summer Temp", "Annual Mean Q"," Distance (km)")
names(temp.labs) <- c("annual_avg", "summer_avg", "qmean_raw", "DistKM")

H1 <- ggplot(env_data, aes(x=Value)) +
  geom_histogram() +
  geom_vline(aes(xintercept = mean(Value)), linetype = "dashed", size = 0.6) +
  facet_grid(Connectivity~Variable, scales = "free", labeller = labeller(Connectivity = supp.labs, Variable = temp.labs) ) +
  scale_y_continuous(name="Frequency", labels = comma) 

H1

file.name1 <- paste0(out.dir, "Env_Hists.jpg")
ggsave(H1, filename=file.name1, dpi=300, height=5, width=6)

# Model: functional synchrony --------------------------------------------------------

## analysis: functional synchrony ~ distance + connectivity + functional diversity + environmental synchrony

sizeSync <- allsync %>%
  filter(Trait == "AVG_MXL")

head(sizeSync)

fs <- sizeSync$Sync ## functional synchrony

d <- (sizeSync$Euclid_Dist_Meters)/1000 ## distance in KM

c <- sizeSync$Connectivity

dv <- sizeSync$diversity

tav <- sizeSync$annual_avg

## check distribution

hist(fs)
hist(d)
hist(dv)
hist(tav)

### quick and dirty model

mod1 <- lm(fs~d*c+dv+tav)
summary(mod1)
library(lme4)
mod_mixed = lmer(Sync ~ DistKM * Connectivity + diversity + annual_avg + (1 | Region), data = sizeSync)

summary(mod_mixed)

confint(mod_mixed)

library(merTools)
# install.packages("merTools")

predictInterval(mod_mixed)   # for various model predictions, possibly with new data

REsim(mod_mixed)             # mean, median and sd of the random effect estimates

plotREsim(REsim(mod_mixed))  # plot the interval estimates

library(car)
install.packages("car")

Anova(mod_mixed)



# plotting over distance----------------------------------------------------------------
library(scales)
head(allsync)

supp.labs <- c("Within Basin", "Between Basin")
names(supp.labs) <- c("1","0")

allsync <- allsync %>%
  mutate(Connectivity = factor(Connectivity, levels = c(1, 0)))

## trait sync over distance
S1 <- ggplot(allsync, aes(x=DistKM, y=Sync, color = Trait)) +
  geom_smooth(method = "lm") +
  facet_wrap(~Connectivity, labeller = as_labeller(supp.labs)) +
  scale_color_discrete(name = "Trait", 
                       labels = c("Fecundity", "Max. Length", "Flow Preference", "Temp Preference")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(name="Synchrony") +
  scale_x_log10(name="Eucliean Distance (km)", labels = comma) 

S1

file.name1 <- paste0(out.dir, "Trait_sync_distance.jpg")
ggsave(S1, filename=file.name1, dpi=300, height=5, width=6)

## make all vars long for all on one plot

allsync_one <- allsync %>%
  pivot_wider(names_from = Trait, values_from = Sync) %>%
  pivot_longer(c(annual_avg:qmin_raw, AVG_FECUND:Tp_pref), names_to="Variable", values_to = "Sync")  %>%
  mutate(Variable = factor(Variable, levels = c("AVG_FECUND", "AVG_MXL", "Q_pref", "Tp_pref", "annual_avg", "summer_avg",
                                                "qmean_raw", "qmax_raw", "qmin_raw"))) %>%
  mutate(Type = case_when(Variable %in% c("annual_avg", "summer_avg") ~ "Temperature",
                          Variable %in% c("qmean_raw", "qmax_raw", "qmin_raw") ~ "Flow",
                          Variable %in% c("AVG_FECUND", "AVG_MXL", "Q_pref", "Tp_pref") ~ "Trait"))

  

## all sync over distance
S1a <- ggplot(filter(allsync_one, !Variable %in% c("qmax_raw", "qmin_raw")), aes(x=DistKM, y=Sync, color = Variable, linetype = Type)) +
  geom_smooth(method = "lm") +
  facet_wrap(~Connectivity, labeller = as_labeller(supp.labs)) +
  scale_colour_discrete(name  ="Variable",
                          breaks=c("AVG_FECUND", "AVG_MXL", "Q_pref", "Tp_pref", "annual_avg", "summer_avg", 
                                   "qmean_raw", "qmax_raw", "qmin_raw"),
                          labels=c("Fecundity", "Max. Length", "Flow Preference", "Temp Preference", 
                                   "Annual Temp", "Summer Temp", "Annual Mean Q", "Annual Max Q", "Annual Min Q")) +
  scale_linetype_discrete(name  ="Type",
                          breaks=c("Trait", "Temperature", "Flow"),
                          labels=c("Trait", "Temperature", "Flow")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(name="Synchrony") +
  scale_x_log10(name="Eucliean Distance (km)", labels = comma) 

S1a


file.name1 <- paste0(out.dir, "All_sync_distance.jpg")
ggsave(S1a, filename=file.name1, dpi=300, height=5, width=6)

# plotting func sync v env sync -------------------------------------------


## format data - make env sync long

unique(allsync$Trait)

## define varibale names and factor levels
allsync_long <- allsync %>%
  pivot_longer(annual_avg:qmin_raw, names_to = "EnvMetric", values_to = "EnvSync") %>%
  mutate(Type = case_when(EnvMetric %in% c("annual_avg", "summer_avg") ~ "Temperature",
                             EnvMetric %in% c("qmean_raw", "qmax_raw", "qmin_raw") ~ "Flow")) %>%
  mutate(EnvMetric = factor(EnvMetric, levels = c("annual_avg", "summer_avg", "qmean_raw", "qmax_raw", "qmin_raw")))


## define labels
temp.labs <- c("Annual Temp", "Summer Temp", "Annual Mean Q")
names(temp.labs) <- c("annual_avg", "summer_avg", "qmean_raw")

# flow.labs <- c( "Annual Mean Q", "Annual Max Q", "Annual Min Q")
# names(flow.labs) <- c( "qmean_raw", "qmax_raw", "qmin_raw")

## functional sync vs env sync

S2a <- ggplot(filter(allsync_long, !EnvMetric %in% c("qmax_raw", "qmin_raw")),
              aes(x=EnvSync, y=Sync, color = Trait)) +
  geom_smooth(method = "lm") +
  facet_grid(EnvMetric ~Connectivity , labeller = labeller(Connectivity = supp.labs, EnvMetric = temp.labs),
                                                           scales = "free_x") +
  scale_color_discrete(name = "Trait", 
                       labels = c("Fecundity", "Max. Length", "Flow Preference", "Temp Preference")) +
  scale_y_continuous(name="Functional Synchrony") +
  scale_x_continuous(name="Environmental Synchrony") 

S2a

file.name1 <- paste0(out.dir, "Trait_env_sync.jpg")
ggsave(S2a, filename=file.name1, dpi=300, height=5, width=6)


# Synchrony vs func dist and diversity ------------------------------------

head(allsync_func)

## define labels
trait.labs <- c("Fecundity", "Max. Length", "Flow Preference", "Temp Preference")
names(trait.labs) <- c("AVG_FECUND", "AVG_MXL",    "Q_pref" ,    "Tp_pref")

S3a <- ggplot(allsync,aes(x=distance, y=Sync, color = Trait)) +
  geom_smooth(method = "lm") +
  facet_grid(Connectivity ~Trait, labeller = labeller(Connectivity = supp.labs, Trait = trait.labs),
             scales = "free") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(name="Functional Synchrony") +
  scale_x_continuous(name="Functional Distance", labels = comma) 

S3a

file.name1 <- paste0(out.dir, "func_sync_dist.jpg")
ggsave(S3a, filename=file.name1, dpi=300, height=5, width=6)

S3b <-  ggplot(allsync,aes(x=diversity, y=Sync, color = Trait)) +
  geom_smooth(method = "lm") +
  facet_grid(Connectivity ~Trait, labeller = labeller(Connectivity = supp.labs, Trait = trait.labs),
             scales = "free") +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(name="Functional Synchrony") +
  scale_x_continuous(name="Functional diversity", labels = comma) 

S3b

file.name1 <- paste0(out.dir, "func_sync_div.jpg")
ggsave(S3b, filename=file.name1, dpi=300, height=5, width=6)


head(allsync)


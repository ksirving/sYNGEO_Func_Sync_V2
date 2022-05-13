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


## analysis: functional synchrony ~ distance + connectivity + functional diversity + environmental synchrony

## models per region, per trait

head(allsync)
unique(allsync$Region)

EurSync <- allsync %>% filter(Region == "Europe")
AusSync <- allsync %>% filter(Region == "Oceania")
USASync <- allsync %>% filter(Region == "USA")


# cor of func synchrony ---------------------------------------------------

head(EurSync)

unique(EurSync$Trait)

EurSyncWide <- EurSync %>% select(Trait, Sync, Pair) %>%
  pivot_wider(names_from = Trait, values_from = Sync) %>%
  ungroup() %>% select(-Region, -Pair)

EurSyncWide

cor(EurSyncWide, use = "complete.obs")

#             AVG_FECUND   AVG_MXL    Q_pref   Tp_pref
# AVG_FECUND  1.0000000 0.3174599 0.3126480 0.3337870
# AVG_MXL     0.3174599 1.0000000 0.5025692 0.5203301
# Q_pref      0.3126480 0.5025692 1.0000000 0.4668910
# Tp_pref     0.3337870 0.5203301 0.4668910 1.0000000

AusSyncWide <- AusSync %>% select(Trait, Sync, Pair) %>%
  pivot_wider(names_from = Trait, values_from = Sync) %>%
  ungroup() %>% select(-Region, -Pair)

AusSyncWide

cor(AusSyncWide, use = "complete.obs")

## body size cor with fecundity 0.71

#               AVG_FECUND    AVG_MXL     Q_pref      Tp_pref
# AVG_FECUND  1.000000000 0.71103332 0.07790161 -0.002620223
# AVG_MXL     0.711033324 1.00000000 0.10336175  0.047301944
# Q_pref      0.077901607 0.10336175 1.00000000  0.200419599
# Tp_pref    -0.002620223 0.04730194 0.20041960  1.000000000

USASyncWide <- USASync %>% select(Trait, Sync, Pair) %>%
  pivot_wider(names_from = Trait, values_from = Sync) %>%
  ungroup() %>% select(-Region, -Pair)

USASyncWide

cor(USASyncWide, use = "complete.obs")

#             AVG_FECUND   AVG_MXL    Q_pref   Tp_pref
# AVG_FECUND  1.0000000 0.6171581 0.2663859 0.2312989
# AVG_MXL     0.6171581 1.0000000 0.1797223 0.1775311
# Q_pref      0.2663859 0.1797223 1.0000000 0.1926675
# Tp_pref     0.2312989 0.1775311 0.1926675 1.0000000

# data distribution  and correlation  -------------------------------------------------------------

sizeSync <- EurSync %>%
  filter(Trait == "AVG_MXL")

# save(sizeSync, file = "output_data/02_europe_body_size_data_for_model_explo.RData")

load( file = "output_data/02_europe_body_size_data_for_model_explo.RData") ## sizeSync

head(sizeSync)
fs <- sizeSync$Sync ## functional synchrony

d <- (sizeSync$Euclid_Dist_Meters)/1000 ## distance in KM

c <- sizeSync$Connectivity

dv <- sizeSync$diversity

di <- sizeSync$distance

tav <- sizeSync$annual_avg


# Model exploration: LM -------------------------------------------------------

## check distribution, too many points for shapiro wilk 
# install.packages("e1071")
library(e1071)
skewness(na.omit(fs)) ## -0.19 - OK, log transformed not ok
skewness(d^(1/3)) ## 0.65 - mod skewed, NaN with log, -0.37 with cube root
skewness(na.omit(dv)) ## 0.4 - ok
skewness(na.omit(log(di))) ## 2.18 = highly skewed, with log transform = 0.07
skewness(na.omit(tav^(1/2))) ## -0.95 = highly skewed negative, log makes it worse -1.17, cube root still bad -1.09, sq root bad -1.06

## positively skewed
hist(d^(1/3))

d <- d^(1/3)

## ok
hist(dv)
hist(log(di)+1)

di <- log(di)+1

## negatively skewed
hist(tav)
hist(log10(tav)+1)
hist(tav^(1/3))

## correlation

data_log <- cbind(fs,d,c,dv,di,tav)
head(data_log)

cor(data, use = "complete.obs")

## distance km and avg temp = -0.82

## removed distance


## estimate synchrony in traits based on functional distance, diversity, euclidean distance, connectivity and environmental synchrony
head(data_log)
### simplest model

mod1 <- lm(fs~c+dv+di+tav)

## QQ plots
plot(mod1) # qq has big tails

summary(mod1) ## all significant 
anova(mod1)

## interactions with environmental synchrony
mod2 <- lm(fs~(c+dv+di)*tav)
mod2a <- lm(log(fs)~(c+dv+di)*tav) ## log response to check, qq plot and r sq much worse
# summary(mod2a)

summary(mod2) ## connectivity and functional distance single effects not significant, functional diversity and env sync significant
## but not interaction, functional distance interacts with environmental synchrony
anova(mod2)

summary(mod2)$coefficient ## functional diversity, av temp and interaction of functional distance and av temp significant

qqnorm(mod2a$residuals)
qqline(mod2a$residuals)
## env sync is highly skewed with no help from log etc transformation
## low R sq
## median not far from zero but slightly left skewed - not predicting lower values as well?

## box cox transfrmation
library(MASS)

bc <- boxcox(fs ~ tav)
(lambda <- bc$x[which.max(bc$y)]) ##  1.070707

mod3 <- lm(((fs^lambda-1)/lambda)~(c+dv+di)*tav)


summary(mod3)
anova(mod3)

summary(mod3)$coefficient 

qqnorm(mod3$residuals)
qqline(mod3$residuals) ## still way off normality

## plot
xseq <- seq(0, 1, length.out = 20) 
coefs <- coef(mod2)
title <- substitute("Multi Linear Regression")
plot(xseq, coefs[1] + coefs[2] * xseq, type = "l", ylim = c(0, 1),
     xlab = expression(italic(x)), ylab = expression(hat(italic(y))),
     main = title, lwd = 2, col = "blue", cex.lab = 1.25, cex.main = 1.5)

## reduce model

mod3 <- lm(fs~(c + di)*tav)

summary(mod3)
summary(mod3)$coefficient


# Model exploration: GLM --------------------------------------------------
head(sizeSync)
fs <- sizeSync$Sync ## functional synchrony

d <- (sizeSync$Euclid_Dist_Meters)/1000 ## distance in KM

c <- sizeSync$Connectivity

dv <- sizeSync$diversity

di <- sizeSync$distance

tav <- sizeSync$annual_avg

## use non transformed data
data <- cbind(fs,d,c,dv,di,tav)

## or transformed 
head(data_log)

data <- as.data.frame(data)
head(data)

mod <- glm(fs~(c+dv+di)*tav ,data=data,family=Gamma(link = "log"))

summary(mod)
anova(mod)

summary(mod)$coefficient ## functional diversity, av temp and interaction of functional diversity and av temp significant

# qqnorm(mod$residuals)
# qqline(mod$residuals) ##
# 
# plot(mod)

## helpful youtube vid on flexplot https://www.youtube.com/watch?v=HmMag6EvNyQ&ab_channel=QuantPsych
## using flexplot
# devtools::install_github("dustinfife/flexplot")
require(flexplot)
?flexplot
## distirbution
flexplot(fs~1, data=data)

##gamma plot - this bins diversity which i don't think we want
flexplot(fs ~ di+dv+tav|c, data=data, method = "Gamma")

# full model 
mod <- glm(fs~(c+dv+di)*tav ,data=data,family=Gamma(link = "log"))
summary(mod)
# anova(mod)


## connectivity and av temp just to check
mod0 <- glm(fs~c*tav ,data=data,family=Gamma(link = "log"))
summary(mod0) ## reduced  
# anova(mod)

## remove connectivity
mod2 <- glm(fs~(dv+di)*tav ,data=data,family=Gamma(link = "log"))
summary(mod2)
anova(mod2)

## remove func distance
mod3 <- glm(fs~(dv)*tav ,data=data,family=Gamma(link = "log"))
summary(mod3)
anova(mod3)

## func distance and av temp
mod4 <- glm(fs~(di)*tav ,data=data,family=Gamma(link = "log"))
summary(mod4)  ## AIC drastically reduced - this might be the best one?!
anova(mod4)

mod4a <- glm(fs~(di+c)*tav ,data=data,family=Gamma(link = "log"))
summary(mod4a)  ## AIC drastically reduced - similar to mod4, maybe this is the best one?
anova(mod4a)

## only av temp
mod5 <- glm(fs~tav ,data=data,family=Gamma(link = "log"))
summary(mod5) ## similar to mod4
anova(mod5)

## only distance
mod6 <- glm(fs~di ,data=data,family=Gamma(link = "log"))
summary(mod6) ## lowest AIC but no interaction
anova(mod6)

## only diversity
mod7 <- glm(fs~dv ,data=data,family=Gamma(link = "log"))
summary(mod7) ## AIC way higher with functional diversity
anova(mod7)

### compare best models

full <- glm(fs~di*tav ,data=data,family=Gamma(link = "log"))
reduced <- glm(fs~di ,data=data,family=Gamma(link = "log"))
# ?compare.fits
compare.fits(fs ~ di | tav , data = data, full, reduced) 
## synchrony increases with increased temp sync, but doesn't change with only functional distance 
## (with logged di = negative relationship)

model.comparison(full, reduced) ## but here the reduced model with only distance is better

## campare models without interaction
me <- glm(fs~di+tav ,data=data,family=Gamma(link = "log"))
compare.fits(fs ~ di | tav , data = data, full, me) 

model.comparison(full, me) ## model with no interaction is better

## needs a bit more investigating but it looks like synchriny in body size increases with synchrony in av temp
## makes sense
## but also as functional distance increases so does synchrony in body size - this doesn't seem right
## update: with logged distance values relationhsip is negative

## do some more plotting and comparing
full <- glm(fs~(di+c)*tav ,data=data,family=Gamma(link = "log"))
reduced <- glm(fs~di+c ,data=data,family=Gamma(link = "log"))
# ?compare.fits
compare.fits(fs ~ di | c+tav , data = data, full, reduced) 
## not much of an effect but within basins functional synchrony is higher with lower bin of av temp, but the interation
## is making it weird i think

model.comparison(full, reduced) ## the reduced model is better

## campare models without interaction
me <- glm(fs~di+tav +c ,data=data,family=Gamma(link = "log"))
compare.fits(fs ~ di | tav , data = data, full, me) 

model.comparison(full, me) ## model with no interaction is better


# full model with no interaction
modi <- glm(fs~c+dv+di+tav ,data=data,family=Gamma(link = "log"))
summary(modi) ## all have an effect when modelled with no interaction, but AIC is still high
# anova(mod)

## reduce them
modi1 <- glm(fs~dv+di+tav ,data=data,family=Gamma(link = "log"))
summary(modi1) ## all have an effect when modelled with no interaction, but AIC is still high, similar to modi
# anova(mod)

## reduce them
modi2 <- glm(fs~di+tav ,data=data,family=Gamma(link = "log"))
summary(modi2) ## AIC reduced 
# anova(mod)

## reduce them
modi3 <- glm(fs~dv+tav ,data=data,family=Gamma(link = "log"))
summary(modi3) ## AIC high
# anova(mod)

## reduce them
modi4 <- glm(fs~c+di+tav ,data=data,family=Gamma(link = "log"))
summary(modi4) ## all have an effect when modelled with no interaction, but AIC is much lower without diversity
# anova(mod)

## overall results
summary(mod6) ## lowest AIC overall but only 1 predictor variable (functional distance) which has a positive effect 
## update: with logged distance effect is negative
summary(mod4a) ## removing functional diversity has a big impact on AIC, and connectivity has an effect once dv is removed
summary(modi4) ## removing interactions reduces AIC very slightly

library(car)
avPlots(modi4)
range(na.omit(data$di))

# Model exploration: GAM --------------------------------------------------

## useful tutorial on GAMs in R - https://noamross.github.io/gams-in-r-course
library(mgcv)

## use non transformed data
head(sizeSync)
fs <- sizeSync$Sync ## functional synchrony

d <- (sizeSync$Euclid_Dist_Meters)/1000 ## distance in KM

c <- sizeSync$Connectivity

dv <- sizeSync$diversity

di <- sizeSync$distance

tav <- sizeSync$annual_avg

data <- cbind(fs,d,c,dv,di,tav) ## d = euclidean distance, and not used as it correlates 

## or transformed
head(data_log)


head(data)
plot(data)
str(data)

data <- data %>% 
  mutate(c = as.factor(c))

data <- as.data.frame(data)

## start simple, no interactions. 
?gam
# Fit the model
mod_city <- gam(fs ~  s(dv) + s(di) + s(tav), 
                data = mpg, method = "REML")

# Plot the model
plot(mod_city, pages = 1)

# play with the criteria for wiggliness
mod_city <- gam(fs ~ s(dv, k = 20) + s(di, k = 20) + s(tav, k = 20), 
                data = mpg, method = "REML")

mod_city_5 <- gam(fs ~ s(dv, k = 5) + s(di, k = 5) + s(tav, k = 5), 
                data = mpg, method = "REML")

plot(mod_city, pages = 1)
plot(mod_city_5, pages = 1) ## this looks nicer

## extract smoothing parameter
mod_city_5$sp


# test smoothing parameters
# mod_city_s1 <- gam(fs ~ s(dv, k = 5) + s(di, k = 5) + s(tav, k = 5), 
#                 data = mpg, sp = 2)
# 
# mod_city_s2 <- gam(fs ~ s(dv, k = 5) + s(di, k = 5) + s(tav, k = 5), 
#                 data = mpg, sp = 0.00001)
# 
# par(mfrow = c(2, 1))
# plot(mod_city_s1, pages = 1)
# plot(mod_city_s2, residuals = TRUE, pch = 1)


summary(mod_city_5) ## low deviance explained

## add categorical variable - connectivity
mod_sep2 <- gam(fs ~ s(dv, k = 5) + s(di, k = 5) + s(tav, k = 5)  + c,
                data = data, method = "REML")

summary(mod_sep2) ## all significant but low deviance
## plot
plot(mod_sep2, pages = 1, seWithMean = TRUE, shift = coef(mod_sep2)[1])

# Fit a model with separate smooths for each connectivity level - this takes a while!!!
mod_sep <- gam(fs ~ s(dv, k = 50, by = c) + s(di, k = 50, by = c) + s(tav, k = 5, by = c)  + c,
               data = data, method = "REML")

# Examine the summary
summary(mod_sep) ## interesting. connectivity is not significant, all other effects are significant and non linear
## EDF - effective degrees of freedom (smoothness complexity) - 1= linear, 2=quadratic etc
## deviance is low

## plot
plot(mod_sep, pages = 1, seWithMean = TRUE, shift = coef(mod_sep)[1])

## check model
gam.check(mod_sep) ## not good, need no signifiance but changing the k value isn't helping

# # Fit a model with factor-smooth interaction
# mod_fs <- gam(fs ~ s(di, c, bs = "fs"),
#               data = data, method = "REML")
# 
# summary(mod_fs)
# plot(mod_fs, pages = 1, seWithMean = TRUE, shift = coef(mod_fs)[1])


# Model exploration: BRTs (for relative importance only) ---------------------------

library(gbm)
library(dismo)
library(ggplot2)
library(purrr)
library(dplyr)
library(tidyverse)

source("code/functions/My.gbm.step.R") ## function for brt - (C) Ryan Peek

set.seed(321) # reproducibility

## use non transformed data - brts can handle it

data
data <- as.data.frame(data)
head(data)

## remove NAs from response 
data <- data[!is.na(data$fs),]

## gbm functions in brt.functions script

# set up tuning params
hyper_grid <- expand.grid(
  shrinkage = c(0.001, 0.003, 0.005), 
  interaction.depth = c(5), 
  n.minobsinnode = c(3, 5, 10), 
  bag.fraction = c(0.75, 0.8) 
)

# double check and view
hyper_grid

names(data)
?gbm.step
# load the GBM.step function (requires dismo and function loaded)
gbm_fit_step <- function(
  shrinkage, interaction.depth, n.minobsinnode, bag.fraction, data) {
  set.seed(123) # make it reproducible
  m_step <- My.gbm.step(
    gbm.y = 1, # functional sycnrhony
    gbm.x = 2:5, # variables
    family = "gaussian", ## check this!!
    data = data,
    #max.trees = 8000, # can specify but don't for now
    learning.rate = shrinkage,
    tree.complexity = interaction.depth,
    n.minobsinnode = n.minobsinnode,
    bag.fraction = bag.fraction,
    plot.main = FALSE,
    verbose = FALSE
  )
  
  # Compute the Deviance Explained: (total dev - cv dev) / total dev
  if(!is.null(m_step)){ # this helps if there's an error above
    (m_step$self.statistics$mean.null - m_step$cv.statistics$deviance.mean) /
      m_step$self.statistics$mean.null
  } else { 
    return(NA)
  }
}

# use PURRR: this part can take awhile...get some coffee
hyper_grid$dev_explained <-purrr::pmap_dbl(
  hyper_grid,
  ~ gbm_fit_step(
    shrinkage = ..1,
    interaction.depth = ..2,
    n.minobsinnode = ..3,
    bag.fraction = ..4,
    data = data) 
)

# look at results:
hyper_grid %>% 
  dplyr::arrange(desc(dev_explained)) %>%
  head(5) # top 5 models

# pick the best solution
(hyper_best <- hyper_grid %>% 
    dplyr::arrange(desc(dev_explained)) %>% #
    head(n=1))


# based on above, run final BRT and save:
gbm_final_step <- function(
  shrinkage, interaction.depth, n.minobsinnode, bag.fraction, data) {
  set.seed(123) # make it reproducible
  m_final <- My.gbm.step(
    gbm.y = 1, # response in training data
    gbm.x = 2:5, # hydro dat
    family = "gaussian",
    data = data,
    learning.rate = shrinkage,
    tree.complexity = interaction.depth,
    n.minobsinnode = n.minobsinnode,
    bag.fraction = bag.fraction,
    plot.main = TRUE,
    verbose = TRUE
  )
}

# set up filename for best model outputs
(gbm_best_file <- paste0("models/04_gbm_final_model_output.txt"))

# run best option with PURR
capture.output(gbm_fin_out <- purrr::pmap(
  hyper_best,
  ~ gbm_final_step(
    shrinkage = ..1,
    interaction.depth = ..2,
    n.minobsinnode = ..3,
    bag.fraction = ..4,
    data = data # CHECK AND CHANGE!!
  )
), file=gbm_best_file, append=T)

#strip off a list layer to view data
(gbm_fin_out <- gbm_fin_out[[1]])

# add hyperbest to capture output file:
cat("\nBest parameters for GBM.STEP:\n\n", 
    file = gbm_best_file, append=TRUE)

# add the parameters used to run the model
write.csv(hyper_best, "output_data/04_best_model_output.csv")

# % percent explained
(gbm_fin_out$self.statistics$mean.null - gbm_fin_out$cv.statistics$deviance.mean) / gbm_fin_out$self.statistics$mean.null 




# Model exploration: Mixed ------------------------------------------------

## gave up on this one, can revisit if needed
library(lme4)
mod_mixed = lmer(Sync ~ DistKM + Connectivity + diversity * annual_avg + (1 | Site_ID1), data = sizeSync)

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


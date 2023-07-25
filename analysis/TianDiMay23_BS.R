# Leaf N LMM fitting and order --------------------

###
### R script for analysis of n/p
###
### Di / Bernhard
###
### History:
### - 12-04-2021 file created-Bernhard
### - 10-05-2023 changes-Bernhard

options(digits=4)                     # set number of post-decimal digits in output to 4

## libraries:
#install.packages("data.table")
#install.packages("jsonlite")
#install.packages("metafor")
#install.packages("ggplot2")
#install.packages("readxl")
#install.packages("dplyr")
#install.packages("nlme")
#install.packages("lme4")
#install.packages("lmerTest")
#install.packages("effects")
#install.packages("ggeffects")
library(metafor)
library(ggplot2)
library(readxl)
library(dplyr)
library(nlme)
library(lme4)
library(lmerTest)
library(effects)
library(ggeffects)
# library(asreml) #this is not free ##Di has no this, can only use others
# library(pascal) #from Pascal Niklaus to go with asreml
library(readr)

# Read in data and prepare ---------------------

# Desktop Bernhard:
# setwd("C:/Users/Bernhard Schmid/Dropbox/4.6-Veröffentl9June16/4.6.3a-Papers(in-prog.)/S-Z/TianDi23")
# dat <- read.csv("leafnp_data_covariates_20210702.csv")

# beni:
dat <- read_csv("~/data/LeafNP_tiandi/Global_total_leaf_N_P_Di/leafnp_data_covariates_20210702.csv")

## Structure of data set:
str(dat)
names(dat)

## Make factors:
dat$SITE <- factor(dat$sitename);nlevels(dat$SITE)
#dat$REC <- factor(dat$Record_ID)
dat$FG <- factor(dat$FunGroups)
dat$FGID <- factor(dat$Dc_Db_Ec_Eb_Hf_Hg)
dat$GF <- factor(dat$tree_shrub_Herb)
dat$FAN <- factor(dat$Family_New);nlevels(dat$FAN)
dat$FA <- factor(dat$Family);nlevels(dat$FA)
dat$GE <- factor(dat$Genus);nlevels(dat$GE)
dat$SP <- factor(dat$Species);nlevels(dat$SP)
dat$DS <- factor(dat$dataset_);nlevels(dat$DS)
dat$ID <- factor(dat$id);nlevels(dat$ID)

# OLS --------------
## leaf N ~ Family -----------
y <- log(dat$leafN)
lm1 <- lm(y ~ FA, na.action = na.exclude, data = dat)
anova(lm1)
par(mfrow=c(2,2))
plot(lm1)

## leaf N ~ Species + Site -----------
lm2 <- lm(y ~ SP + SITE, na.action = na.exclude, data = dat)  # may take > 1 hour
anova(lm2)
saveRDS(lm2, file = paste0(here::here(), "/data/lm2.rds"))

#Response: y
#             Df Sum Sq Mean Sq F value Pr(>F)    
#SP         3698   3741   1.012   36.08 <2e-16 ***
#SITE       7331    996   0.136    4.85 <2e-16 ***
#Residuals 25382    712   0.028  

## leaf N ~ ENV. + Species (fixed) --------------
lm2b <- lm(y ~ SP + ALSA + mav + elv + co2 + tmonthmin + mai + ndep, na.action = na.exclude, data = dat)
anova(lm2b) 
saveRDS(lm2b, file = paste0(here::here(), "/data/lm2b.rds"))

# These environmental variables only explain about 10% of all variation between sites:
# Response: y
#             Df Sum Sq Mean Sq F value  Pr(>F)    
#SP         3698   3741     1.0   20.50 < 2e-16 ***
#ALSA          1      0     0.4    8.94  0.0028 ** 
#mav           1      5     4.7   95.83 < 2e-16 ***
#elv           1      5     5.4  109.75 < 2e-16 ***
#co2           1      1     1.5   29.41 5.9e-08 ***
#tmonthmin     1     21    21.4  433.21 < 2e-16 ***
#mai           1      2     2.2   45.14 1.9e-11 ***
#ndep          1     59    58.6 1186.86 < 2e-16 ***
#Residuals 32705   1614     0.0 

## leaf N ~ Site + Species ------------------
# opposite order compated to lm2
lm3 <- lm(y ~ SITE + SP, na.action = na.exclude, data = dat)  # may take > 1 hour
anova(lm3)
saveRDS(lm3, file = paste0(here::here(), "/data/lm3.rds"))

#Response: y
#             Df Sum Sq Mean Sq F value Pr(>F)    
#SITE       7549   3882   0.514   18.34 <2e-16 ***
#SP         3480    855   0.246    8.76 <2e-16 ***
#Residuals 25382    712   0.028

## leaf N ~ ENV. + Species -----------------------
# opposite order compared to lm2b
lm3b <- lm(y ~ ALSA + mav + elv + co2 + tmonthmin + mai + ndep + SP, na.action = na.exclude, data = dat)
anova(lm3b) 

# And here they only explain <5% of all variation between sites:
#Response: y
#             Df Sum Sq Mean Sq F value  Pr(>F)    
#ALSA          1     67    66.5 1348.70 < 2e-16 ***
#mav           1      1     1.3   26.04 3.4e-07 ***
#elv           1      7     7.1  143.24 < 2e-16 ***
#co2           1      3     3.1   63.40 1.7e-15 ***
#tmonthmin     1      1     1.3   26.18 3.1e-07 ***
#mai           1      0     0.2    3.72   0.054 .  
#ndep          1     75    74.9 1518.58 < 2e-16 ***
#SP         3698   3681     1.0   20.17 < 2e-16 ***
#Residuals 32705   1614     0.0 

# LMM -------------------
asreml.options(workspace="10gb")

## Only random factors -------------
asr1 <- asreml(fixed = y ~ 1
               ,random = ~SITE + SP + SITE:SP
               ,na.action=na.method(x="omit",y="omit")
               ,data=dat)  #[dat$matgs>0,])
test.asreml(asr1)

#---- Stratum variances:
#            df Variance   SP   SITE SITE:SP units!R
#SP        3889  0.22158 1.84 0.6117   1.337       1
#SITE      6127  0.10353 0.00 1.9273   1.347       1
#SITE:SP   3853  0.04875 0.00 0.0000   1.620       1
#units!R  22542  0.02455 0.00 0.0000   0.000       1
#---- Variance components:
#        component std.error z.ratio bound %ch
#SP        0.08608 0.0027705   31.07     P   0
#SITE      0.03054 0.0010827   28.21     P   0
#SITE:SP   0.01494 0.0007005   21.33     P   0
#units!R   0.02455 0.0002312  106.17     P   0
plot(asr1) # still many outliers

## ENV as fixed, then random -------------
asr1a <- asreml(fixed = y ~ ALSA + mav + elv + co2 + tmonthmin + mai + ndep
                ,random = ~SP + SITE + SP:SITE
                ,na.action = na.method(x="omit", y="omit")
                ,data = dat)
test.asreml(asr1a) 

# Correcting for species ID in random part makes influece of env. variables small:
#---- Wald tests:
#            Df denDF  F.inc      Pr    
#ALSA         1  6780      0    0.99    
#mav          1  7449    202 < 2e-16 ***
#elv          1  9809      1    0.25    
#co2          1  7366     22 2.3e-06 ***
#tmonthmin    1  8135     25 7.2e-07 ***
#mai          1  7359    126 < 2e-16 ***
#ndep         1  6750    186 < 2e-16 ***
#---- Stratum variances:
#           df Variance    SP   SITE SP:SITE units!R
#SP       3848  0.21830 1.865 0.6076   1.354       1
#SITE     6125  0.09595 0.000 1.9320   1.335       1
#SP:SITE  3909  0.04846 0.000 0.0000   1.614       1
#units!R 22520  0.02459 0.000 0.0000   0.000       1
#---- Variance components:
#        component std.error z.ratio bound %ch
#SP        0.08441 0.0027053   31.20     P   0
#SITE      0.02671 0.0010130   26.37     P   0
#SP:SITE   0.01479 0.0006943   21.30     P   0
#units!R   0.02459 0.0002318  106.11     P   0

## First Species -------------
asr1b <- asreml(fixed = y ~ SP + ALSA + mav + elv + co2 + tmonthmin + mai + ndep
                , random = ~SITE + SP:SITE
                , na.action = na.method(x = "omit", y = "omit")
                , data = dat
                )
test.asreml(asr1b) 

# Fitted after species ID the influence of environmental variables is also small:

#---- Wald tests:
#              Df denDF  F.inc      Pr    
#SP          3699             8            
#ALSA           1  6288       5  0.0255 *  
#mav            1  6936      95 < 2e-16 ***
#elv            1  9026      35 3.3e-09 ***
#co2            1  6189       1  0.3847    
#tmonthmin      1  8533      10  0.0017 ** 
#mai            1  6825      54 1.8e-13 ***
#ndep           1  6616     179 < 2e-16 ***
#---- Stratum variances:
#           df Variance SITE SP:SITE units!R
#SITE     6536  0.08155  1.9   1.378       1
#SP:SITE  3738  0.05234  0.0   1.548       1
#units!R 22429  0.02466  0.0   0.000       1
#---- Variance components:
#        component std.error z.ratio bound %ch
#SITE      0.01697 0.0009414   18.03     P   0
#SP:SITE   0.01789 0.0007966   22.46     P   0
#units!R   0.02466 0.0002328  105.90     P   0
#Warning message:
#  In asreml(fixed = y ~ SP + ALSA + mav + elv + co2 + tmonthmin +  :
#              Log-likelihood not converged

## Last Species --------------
asr1c <- asreml(fixed = y ~ ALSA + mav + elv + co2 + tmonthmin + mai + ndep + SP
                ,random = ~SITE + SP:SITE
                ,na.action = na.method(x = "omit", y = "omit")
                ,data = dat)
test.asreml(asr1c) 

# If not corrected for species ID, the influence of env. variables is much higher:

#---- Wald tests:
#              Df denDF  F.inc      Pr    
#ALSA           1  5231      18 1.9e-05 ***
#mav            1  4357     386 < 2e-16 ***
#elv            1  4724     308 < 2e-16 ***
#co2            1  3207      51 1.1e-12 ***
#tmonthmin      1  3336     728 < 2e-16 ***
#mai            1  4299     283 < 2e-16 ***
#ndep           1  4977     455 < 2e-16 ***
#SP          3699  4130       8 < 2e-16 ***
#---- Stratum variances:
# df Variance SITE SP:SITE units!R
#SITE     6536  0.08155  1.9   1.378       1
#SP:SITE  3738  0.05234  0.0   1.548       1
#units!R 22429  0.02466  0.0   0.000       1
#---- Variance components:
#        component std.error z.ratio bound %ch
#SITE      0.01697 0.0009414   18.03     P   0
#SP:SITE   0.01789 0.0007966   22.46     P   0
#units!R   0.02466 0.0002328  105.90     P   0




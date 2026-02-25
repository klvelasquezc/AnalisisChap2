#
# Analysis chapter 2
# Graphical Causal Model - GCM
# Karen Velasquez-C
# 05-11-2025
#
#######################

## Libraries ----

library(DHARMa)
library(AICcmodavg)
library(ggplot2)
library(tidyverse)
library(glmmTMB)
library(piecewiseSEM)


## Data ----

chap2 <- read.csv("Data/dataSEM.csv", header = T) 

## Abundance models ----

summary(ModAbu1 <- (glmmTMB(RodAbu~IE250, data = chap2, family = nbinom2())))
summary(ModAbu2 <- (glmmTMB(RodAbu~IE250+PredAbun, data = chap2, family = nbinom2())))
summary(ModAbu3 <- (glmmTMB(RodAbu~IE250+EVI, data = chap2, family = nbinom2())))
summary(ModAbu4 <- (glmmTMB(RodAbu~IE250+Slope, data = chap2, family = nbinom2())))
summary(ModAbu5 <- (glmmTMB(RodAbu~IE250+ProxIE0.5, data = chap2, family = nbinom2())))
summary(ModAbu6 <- (glmmTMB(RodAbu~IE250+ProxIE0.5+Slope+EVI, data = chap2, family = nbinom2())))

## Modelos candidatos

cand_Abun <- list(Mod1,Mod2,Mod3,Mod4,Mod5,Mod6)
names_Abun <- c("RodAbu~IE250",
              "RodAbu~IE250+PredAbun",
              "RodAbu~IE250+EVI",
              "RodAbu~IE250+Slope",
              "RodAbu~IE250+ProxIE0.5",
              "RodAbu~IE250+ProxIE0.5+Slope+EVI"
              )

## Selección de modelos

AICc_Abun <- aictab(cand_Abun,
                  modnames = names_Abun,
                  second.ord = T,
                  sort = T)


## Test dispersión y Zero inflado

# Modelo seis RodAbu~IE250+ProxIE0.5+Slope+EVI

testDispersion(Mod6)
Mod6_residuals <- simulateResiduals(Mod6)
plot(Mod6_residuals)
testZeroInflation(Mod6)

# Modelo cinco RodAbu~IE250+ProxIE0.5

testDispersion(Mod5)
Mod5_residuals <- simulateResiduals(Mod5)
plot(Mod5_residuals)
testZeroInflation(Mod5)

# Modelo uno RodAbu~IE250

testDispersion(Mod1)
Mod1_residuals <- simulateResiduals(Mod1)
plot(Mod1_residuals)
testZeroInflation(Mod1)


## Diversity models ----

summary(ModDiv1 <- (glmmTMB(Rodq1~IE250, data = chap2, family = gaussian())))
summary(ModDiv2 <- (glmmTMB(Rodq1~IE250+Slope, data = chap2, family = gaussian())))
summary(ModDiv3 <- (glmmTMB(Rodq1~IE250+EVI, data = chap2, family = gaussian())))
summary(ModDiv4 <- (glmmTMB(Rodq1~IE250+Slope+EVI, data = chap2, family = gaussian())))

## Modelos candidatos

cand_Div <- list(ModDiv1,ModDiv2,ModDiv3,ModDiv4)
names_Div <- c("Rodq1~IE250",
                "Rodq1~IE250+Slope",
                "Rodq1~IE250+EVI",
                "Rodq1~IE250+Slope+EVI"
)

## Selección de modelos

AICc_Div <- aictab(cand_Div,
                    modnames = names_Abun,
                    second.ord = T,
                    sort = T)

## Test dispersión y Zero inflado

# Modelo uno Rodq1~IE250

testDispersion(ModDiv1)
ModDiv1_residuals <- simulateResiduals(ModDiv1)
plot(ModDiv1_residuals)
testZeroInflation(ModDiv1)

# Modelo cuatro Rodq1~IE250+EVI-+Slope

testDispersion(ModDiv4)
ModDiv4_residuals <- simulateResiduals(ModDiv4)
plot(ModDiv4_residuals)
testZeroInflation(ModDiv4)



####################################
##
##  Modelos IE - Pequeños mamíferos
##  Bayesiano
##  Karen Velasquez-C 
##  11/02/2026
##
#####################################

# library -----------------------------------------------------------------

library(tidyverse)
library(brms)
library(tidybayes)
library(performance)
library(broom.mixed)
library(DHARMa)
library(DHARMa.helpers)
library(gt)


# data --------------------------------------------------------------------

chap2 <- read.csv("~/INECOL/Doctorado/Proyecto2020/Capitulo 2/Analisis SEM/Data/dataSEM.csv", header = T) %>% 
  drop_na()


# Models ------------------------------------------------------------------


# Abundance general models ------------------------------------------------


formula_1 <- bf(RodAbu~IE250, hu~as.factor(Season)*as.factor(Cumulo))

Abu1 <- brm(formula_1, data = chap2, family = hurdle_poisson(),
  chains = 4, iter = 10000, warmup = 2000,
  cores = 4
)

summary(Abu1)

simres <- dh_check_brms(Abu1, integer = TRUE)

testDispersion(simres)
testZeroInflation(simres)



# Abundance Peromyscus models --------------------------------------------

formula_Pero <- bf(AbuPero~IE250, hu~as.factor(Season))

AbuPero1 <- brm(formula_Pero, data = chap2, family = hurdle_poisson(),
            chains = 4, iter = 6000, warmup = 1000,
            cores = 4
)

summary(AbuPero1)

simres_Pero <- dh_check_brms(AbuPero1, integer = TRUE)

testDispersion(simres_Pero)
testZeroInflation(simres_Pero)

# Modelo AbuPero

Formula_AbuPred <- bf(AbuPero~IE250+PredAbun)

AbuPero2 <- brm(Formula_AbuPred, data = chap2, family = hurdle_poisson(),
                chains = 4, iter = 6000, warmup = 1000,
                cores = 4
)

summary(AbuPero2)

simres_Pero2 <- dh_check_brms(AbuPero2, integer = TRUE)

testDispersion(simres_Pero2)
testZeroInflation(simres_Pero2)


# Richness models --------------------------------------------------------

formula_2 <- bf(Rodq0~IE1000)

Riq1 <- brm(formula_2, data = chap2, family = zero_inflated_poisson(),
            chains = 4, iter = 10000, warmup = 2000,
            cores = 4
)

simres_q0 <- dh_check_brms(Riq1, integer = TRUE)

testDispersion(simres_q0)
testZeroInflation(simres_q0)

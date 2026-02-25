## Ejemplo SEM con menor número de datos ####

## Cargar la paquetería 

## DAG


library(piecewiseSEM)
data("keeley")


## Primeros datos
data <- keeley

## Fit individual models
abiotic <- lm(abiotic ~ distance, data)
hetero <- lm(hetero ~ distance, data)
richness <- lm(rich ~ abiotic + hetero, data)

model <- psem(abiotic, hetero, richness)

# fit indenpendence claims

dsep1 <- lm(abiotic~hetero+ distance, data) 
dsep2 <- lm(rich ~distance + abiotic + hetero, data)

summary(dsep1)
summary(dsep2)

fisherC(model)

model2 <- psem(
  lm(abiotic ~ distance, data),
  lm(hetero ~ distance, data),
  lm(rich ~ abiotic + hetero + distance, data),
  data = data
  )

coefs(model2)

windows()
plot(model2)

summary(model2)

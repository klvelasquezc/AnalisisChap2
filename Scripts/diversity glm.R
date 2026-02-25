# Diversity analysis



# library -----------------------------------------------------------------

library(tidyverse)
library(brms)
library(tidybayes)
library(performance)
library(broom.mixed)
library(gt)


# data --------------------------------------------------------------------

jost_evennes <- read.csv("Data/Jostevennes.csv")

Years <- unique(jost_evennes$Assemblage)
prevY <- unique(jost_evennes$Assemblage)-1


rain_data <- read.csv("Data/Rain/totalrain.csv")


mod_data <- jost_evennes %>% 
  rename(Year= Assemblage) %>% 
  left_join(rain_data)



# Models ------------------------------------------------------------------


# Evennes models ----------------------------------------------------------



get_models <- function(response, family){
  
  # Lista para almacenar los modelos
  model_list <- list()
  
  # Modelo nulo
  model_list$Null <- brm(
    bf(as.formula(paste(response, "~ 1"))),
    data = mod_data,
    family = family,
    chains = 4, iter = 4000, warmup = 1000,
    cores = 4,
    backend = "cmdstanr"
  )
  
  # Modelo con Summer_sameY
Nombre <- brm(
    RodAbu~IE250, data = chap2, family = poisson,
    chains = 4, iter = 4000, warmup = 1000,
    cores = 4
  )
  
  # Modelo con Spring_sameY
  model_list$Spr_sy <- brm(
    bf(as.formula(paste(response, "~ scale(Spring_sameY)"))),
    data = mod_data,
    family = family,
    chains = 4, iter = 4000, warmup = 1000,
    cores = 4,
    backend = "cmdstanr"
  )
  
  # Modelo con sameY
  model_list$Same_y <- brm(
    bf(as.formula(paste(response, "~ scale(sameY)"))),
    data = mod_data,
    family = family,
    chains = 4, iter = 4000, warmup = 1000,
    cores = 4,
    backend = "cmdstanr"
  )
  
  # Modelo con Summer_pastY
  model_list$Sum_py <- brm(
    bf(as.formula(paste(response, "~ scale(Summer_pastY)"))),
    data = mod_data,
    family = family,
    chains = 4, iter = 4000, warmup = 1000,
    cores = 4,
    backend = "cmdstanr"
  )
  
  # Modelo con Spring_pastY
  model_list$Spr_py <- brm(
    bf(as.formula(paste(response, "~ scale(Spring_pastY)"))),
    data = mod_data,
    family = family,
    chains = 4, iter = 4000, warmup = 1000,
    cores = 4,
    backend = "cmdstanr"
  )
  
  # Modelo con Spring_pastY
  model_list$Past_y <- brm(
    bf(as.formula(paste(response, "~ scale(pastY)"))),
    data = mod_data,
    family = family,
    chains = 4, iter = 4000, warmup = 1000,
    cores = 4,
    backend = "cmdstanr"
  )
  
  return(model_list)
}

get_WAIC <- function(models) {
  # Apply the WAIC function to each model in the list
  waic_list <- lapply(models, WAIC)
  
  # Function to extract WAIC values from the WAIC object
  extract_waic <- function(waic_obj, model_name) {
    # Create a data frame with extracted values
    data.frame(
      model = model_name,                # Model name
      elpd_waic = waic_obj$estimates[1, 1],  # Extract elpd_waic (row 1, column 1)
      p_waic = waic_obj$estimates[2, 1],     # Extract p_waic (row 2, column 1)
      waic = waic_obj$estimates[3, 1]        # Extract waic (row 3, column 1)
    )
  }
  
  # Map the extraction function over the WAIC list and model names
  results <- map2_df(waic_list, names(models), extract_waic) %>%
    mutate(
      minWAIC = min(waic),            # Add column for minimum WAIC
      deltaWAIC = waic - minWAIC      # Add column for WAIC difference
    ) %>%
    rename(
      elpd = elpd_waic,               # Rename elpd_waic to elpd
      pD = p_waic,                    # Rename p_waic to pD
      WAIC = waic                     # Rename waic to WAIC
    )
  
  # Extract coefficients for each model
  coeficientes_df <- models %>%
    imap_dfr(~ {
      if (.y == "Null") {
        # If the model is Null, return NA values
        return(data.frame(model = .y, term = NA, estimate = NA, conf.low = NA, conf.high = NA))
      } else {
        # Extract coefficients containing "scale"
        tidy(.x, conf.level = 0.89) %>%
          slice(2) %>%  # Assuming the desired coefficient is in the second row
          mutate(model = .y) %>%
          select(model, estimate, conf.low, conf.high)
      }
    })
  
  # Merge WAIC results and coefficients
  resultados_total <- results %>%
    left_join(coeficientes_df, by = "model") %>%  # Join by model name
    select(-term, -minWAIC)                      # Drop unnecessary columns
  
  # Return the final data frame
  return(resultados_total)
}



# Evennes 1
E1_models <- get_models("Evenness_1", Beta())

E1_compare <- get_WAIC(E1_models)
  
#Evennes 2
E2_models <- get_models("Evenness_2", Beta())

E2_compare <- get_WAIC(E2_models)

# Richness

Q0_models <- get_models(response="TD_asy_Species.richness", 
                        family=gaussian())

Q0_compare <- get_WAIC(Q0_models )

# Diversity q1

Q1_models <- get_models(response= "TD_asy_Shannon.diversity", 
                        family=gaussian())

Q1_compare <- get_WAIC(Q1_models )


# Diversity q2

Q2_models <- get_models(response= "TD_asy_Simpson.diversity", 
                        family=gaussian())

Q2_compare <- get_WAIC(Q2_models )


# AIC tables --------------------------------------------------------------

WAIctab <- rbind(E1_compare, E1_compare,
                 Q0_compare, Q1_compare,
                 Q2_compare) %>% 
  mutate(Variable= rep(c("Evennes profile 1 (q1)",
                       "Evennes profile 2(q2)",
                       "Species Richness (q0)",
                       "Diversity profile 1 (q1)",
                       "Diversity profile 2 (q2)"),
                     each=7
                      )) %>% 
  rename(Model= model)

table <- WAIctab %>% 
  group_by(Variable) %>% 
  arrange(WAIC) %>% 
  gt() %>% 
  fmt_number(decimals = 2)
  
 

table

gtsave(table, "results/WAIC_diversitytable.docx")

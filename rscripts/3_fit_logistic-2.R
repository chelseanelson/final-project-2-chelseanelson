# Final Project II ----
# Define and fit feature engineering logistic regression model

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(doMC)

# handle common conflicts
tidymodels_prefer()

# set up parallel processing 
num_cores <- parallel::detectCores(logical = TRUE)
registerDoMC(cores = num_cores)

# load folded data
load(here("data/model_data/diabetes_folds.rda"))

# load pre-processing/feature engineering/recipe
load(here("data/recipes/feature_eng_rec.rda"))

# model specifications ----
logistic_model <-
  logistic_reg() %>%
  set_mode("classification") %>%
  set_engine("glm")

# define workflows ----
logistic_wflow <-
  workflow() %>% 
  add_model(logistic_model) %>% 
  add_recipe(feature_eng_rec)

# fit workflows/models ----

fit_folds_logistic_2 <- fit_resamples(
  logistic_wflow,
  resamples = diabetes_folds,
  control = control_resamples(save_workflow = TRUE)
)

# write out results (fitted/trained workflows) ----
save(fit_folds_logistic_2, file = here("data/fitted_models/fit_folds_logistic_2.rda"))
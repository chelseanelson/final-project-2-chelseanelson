# Final Project II ----
# Define and fit logistic regression model

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
load(here("data/recipes/baseline_rec.rda"))

# model specifications ----
logistic_model <-
  logistic_reg() %>%
  set_mode("classification") %>%
  set_engine("glm")

# define workflows ----
logistic_wflow <-
  workflow() %>% 
  add_model(logistic_model) %>% 
  add_recipe(baseline_rec)

# fit workflows/models ----
keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

fit_folds_logistic <- fit_resamples(
  logistic_wflow,
  resamples = diabetes_folds,
  control = keep_pred
)

# write out results (fitted/trained workflows) ----
save(fit_folds_logistic, file = here("data/fitted_models/fit_folds_logistic.rda"))
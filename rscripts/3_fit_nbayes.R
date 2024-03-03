# Final Project II ----
# Define and fit baseline models (naive bayes and logistic regression)

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(discrim)
library(doMC)
library(klaR)

# handle common conflicts
tidymodels_prefer()

# set up parallel processing 
num_cores <- parallel::detectCores(logical = TRUE)
registerDoMC(cores = num_cores)

# load folded data
load(here("data/model_data/diabetes_folds.rda"))

# load pre-processing/feature engineering/recipe
load(here("data/recipes/baseline_naive_bayes_rec.rda"))
load(here("data/recipes/baseline_rec.rda"))

# model specifications ----
nbayes_model <-
  naive_Bayes() %>%
  set_mode("classification") %>%
  set_engine("klaR")

# define workflows ----
nbayes_wflow <-
  workflow() %>% 
  add_model(nbayes_model) %>% 
  add_recipe(baseline_naive_bayes_rec)

# fit workflows/models ----
keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

fit_folds_nbayes <- fit_resamples(
  nbayes_wflow,
  resamples = diabetes_folds,
  control = keep_pred
)

# write out results (fitted/trained workflows) ----
save(fit_folds_nbayes, file = here("data/fitted_models/fit_folds_nbayes.rda"))


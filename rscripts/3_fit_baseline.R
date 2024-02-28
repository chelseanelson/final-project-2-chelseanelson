# Final Project II ----
# Define and fit baseline models (naive bayes and logistic regression)

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(discrim)

# handle common conflicts
tidymodels_prefer()

# set up parallel processing 
num_cores <- parallel::detectCores(logical = TRUE)
registerDoMC(cores = num_cores)

# load training data
load(here("data/model_data/diabetes_train.rda"))
load(here("data/model_data/diabetes_folds.rda"))

# load pre-processing/feature engineering/recipe
load(here("data/recipes/baseline_naive_bayes_rec.rda"))
load(Here("data/recipes/baseline_rec.rda"))

################################################################################
# Naive Bayes Model ----
################################################################################

# model specifications ----
nb_model <-
  naive_Bayes() %>%
  set_mode("classification") %>%
  set_engine("klaR")

# define workflows ----
nb_wflow <-
  workflow() %>% 
  add_model(nb_model) %>% 
  add_recipe(baseline_naive_bayes_rec)

# fit workflows/models ----
keep_pred <- control_resamples(save_pred = TRUE)

fit_folds_nb <- fit_resamples(
  nb_wflow,
  resamples = diabetes_folds,
  control = keep_pred
)

##########################################################################
# Logistic Regression Model ----
##########################################################################

# model specifications ----
lg_model <-
  logistic_reg() %>%
  set_mode("classification") %>%
  set_engine("glm")

# define workflows ----
lg_wflow <-
  workflow() %>% 
  add_model(lg_model) %>% 
  add_recipe(baseline_rec)

# fit workflows/models ----
keep_pred <- control_resamples(save_pred = TRUE)

fit_folds_lg <- fit_resamples(
  lg_wflow,
  resamples = diabetes_folds,
  control = keep_pred
)

# write out results (fitted/trained workflows) ----
save(fit_folds_lg, file = here("data/fitted_data/fit_folds_lg.rda"))
save(fit_folds_nb, file = here("data/fitted_data/fit_folds_nb.rda"))


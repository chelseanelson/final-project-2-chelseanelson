# Final Project II ----
# Define, fit, and tune feature engineering netural networks model 
# BE AWARE: there is a random process in this script (seed set right before it)

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(nnet)
library(doMC)

# set up parallel processing 
num_cores <- parallel::detectCores(logical = TRUE)
registerDoMC(cores = num_cores)

# handle common conflicts
tidymodels_prefer()

# load folded data
load(here("data/model_data/diabetes_folds.rda"))

# load pre-processing/feature engineering/recipe
load(here("data/recipes/feature_eng_rec.rda"))

# model specifications ----
nnet_model <- 
  mlp(mode = "classification",
      penalty = tune(),
      epochs = tune(),
      hidden_units = tune()) %>% 
  set_engine("nnet")

# define workflows ----
nnet_wflow <-
  workflow() %>%
  add_model(nnet_model) %>%
  add_recipe(feature_eng_rec)

# hyperparameter tuning values ----

# check ranges for hyperparameters
hardhat::extract_parameter_set_dials(nnet_model)

# save hyperparameter ranges
nnet_params <- extract_parameter_set_dials(nnet_model)
# update stuff now 

# build tuning grid
nnet_grid <- grid_regular(nnet_params, levels = 5)

# fit workflows/models ----

# set seed
set.seed(12352)

nnet_tuned_2 <- 
  nnet_wflow |> 
  tune_grid(
    diabetes_folds, 
    grid = nnet_grid, 
    control = control_grid(save_workflow = TRUE)
  )

# write out results (fitted/trained workflows) ----
save(nnet_tuned_2, file = here("data/tuned_models/nnet_tuned_2.rda"))
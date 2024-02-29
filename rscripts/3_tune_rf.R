# Final Project II ----
# Define and fit random forest model 
# BE AWARE: there is a random process in this script (seed set right before it)

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(ranger)
library(doMC)

# set up parallel processing 
num_cores <- parallel::detectCores(logical = TRUE)
registerDoMC(cores = num_cores)

# handle common conflicts
tidymodels_prefer()

# load folded data
load(here("data/model_data/carseats_folds.rda"))

# load pre-processing/feature engineering/recipe
load(here("data/recipes/diabetes_rec_treebased.rda"))

# model specifications ----
rf_model <- 
  rand_forest(
    mode = "classification",
    trees = 1000, 
    min_n = tune(),
    mtry = tune() #figure out what I want to tune or not 
  ) |> 
  set_engine("ranger")

# define workflows ----
rf_wflow <- 
  workflow() %>% 
  add_model(rf_model) %>%
  add_recipe(diabetes_rec_treebased)

# hyperparameter tuning values ----

# check ranges for hyperparameters
hardhat::extract_parameter_set_dials(rf_model)

# change hyperparameter ranges
rf_params <- extract_parameter_set_dials(rf_model) %>% 
  update(mtry = mtry(c(1, 14)))  # figure out what I want here

# build tuning grid
rf_grid <- grid_regular(rf_params, levels = 5) # and here 

# fit workflows/models ----
# set seed
set.seed(123567)

rf_tuned <- 
  rf_wflow |> 
  tune_grid(
    diabetes_folds, 
    grid = rf_grid, 
    control = control_grid(save_workflow = TRUE)
  )

# write out results (fitted/trained workflows) ----
save(rf_tuned, file = here("data/tuned_models/rf_tuned.rda"))
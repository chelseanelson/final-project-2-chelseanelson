# Final Project II ----
# Define and fit boosted tree model 
# BE AWARE: there is a random process in this script (seed set right before it)

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(xgboost)
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
bt_model <-
  boost_tree(
    mode = "classification",
    mtry = tune(),
    min_n = tune(),
    learn_rate = tune() #figure out what I want to tune 
  ) %>% 
  set_engine("xgboost")

# define workflows ----
bt_wflow <-
  workflow() %>% 
  add_model(bt_model) %>% 
  add_recipe(diabetes_rec_treebased)

# hyperparameter tuning values ----

# check ranges for hyperparameters
hardhat::extract_parameter_set_dials(bt_model)

# change hyperparameter ranges
bt_params <- extract_parameter_set_dials(bt_model) %>% 
  # N:= maximum number of random predictor columns we want to try 
  # should be less than the number of available columns
  update(mtry = mtry(c(1, 14)),
         learn_rate = learn_rate(range = c(-5, -0.2))) # figure out what I want here

# build tuning grid
bt_grid <- grid_regular(bt_params, levels = 5)

# fit workflows/models ----
# set seed
set.seed(12356)

bt_tuned <- 
  bt_wflow |> 
  tune_grid(
    diabetes_folds, 
    grid = bt_grid, 
    control = control_grid(save_workflow = TRUE)
  )

# write out results (fitted/trained workflows) ----
save(bt_tuned, file = here("data/tuned_models/bt_tuned.rda"))
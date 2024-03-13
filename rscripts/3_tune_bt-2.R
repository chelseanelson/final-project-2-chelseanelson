# Final Project II ----
# Define, fit and tune feature engineering boosted tree model 
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
load(here("data/model_data/diabetes_folds.rda"))

# load pre-processing/feature engineering/recipe
load(here("data/recipes/feature_eng_tree_rec.rda"))

# model specifications ----
bt_model <-
  boost_tree(
    mode = "classification",
    mtry = tune(),
    min_n = tune(),
    learn_rate = tune()
  ) %>% 
  set_engine("xgboost")

# define workflows ----
bt_wflow <-
  workflow() %>% 
  add_model(bt_model) %>% 
  add_recipe(feature_eng_tree_rec)

# hyperparameter tuning values ----

# check ranges for hyperparameters
hardhat::extract_parameter_set_dials(bt_model)

# change hyperparameter ranges
bt_params <- extract_parameter_set_dials(bt_model) %>% 
  update(mtry = mtry(c(1, 6)),
         learn_rate = learn_rate(range = c(-5, -0.2)))

# build tuning grid
bt_grid <- grid_regular(bt_params, 
                        levels = c(mtry = 6, learn_rate = 5, 
                                   min_n = 4))

# do learn_rate on a log10 scale for feature engineering 

# fit workflows/models ----
# set seed
set.seed(1235689)

bt_tuned_2 <- 
  bt_wflow |> 
  tune_grid(
    diabetes_folds, 
    grid = bt_grid, 
    control = control_grid(save_workflow = TRUE)
  )

# write out results (fitted/trained workflows) ----
save(bt_tuned_2, file = here("data/tuned_models/bt_tuned_2.rda"))
# Final Project II ----
# Define, fit and tune kitchen sink k-nearest neighbors model 
# BE AWARE: there is a random process in this script (seed set right before it)

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(kknn)
library(doMC)

# set up parallel processing 
num_cores <- parallel::detectCores(logical = TRUE)
registerDoMC(cores = num_cores)

# handle common conflicts
tidymodels_prefer()

# load folded data
load(here("data/model_data/diabetes_folds.rda"))

# load pre-processing/feature engineering/recipe
load(here("data/recipes/baseline_tree_rec.rda"))

# model specifications ----
knn_model <- 
  nearest_neighbor(mode = "classification",
                   neighbors = tune()) %>%
  set_engine("kknn")

# define workflows ----
knn_wflow <-
  workflow() %>%
  add_model(knn_model) %>%
  add_recipe(baseline_tree_rec)

# hyperparameter tuning values ----

# check ranges for hyperparameters
hardhat::extract_parameter_set_dials(knn_model)

# save hyperparameter ranges
knn_params <- extract_parameter_set_dials(knn_model) %>%
  update(neighbors = neighbors(range = c(1,15)))

# build tuning grid
knn_grid <- grid_regular(knn_params, levels = 5)

# fit workflows/models ----

# set seed
set.seed(1235)

knn_tuned <- 
  knn_wflow |> 
  tune_grid(
    diabetes_folds, 
    grid = knn_grid, 
    control = control_grid(save_workflow = TRUE)
  )

# write out results (fitted/trained workflows) ----
save(knn_tuned, file = here("data/tuned_models/knn_tuned.rda"))
# Final Project II ----
# Train final model
# Best Model: 
# BE AWARE: there is a random process in this script (seed set right before it)

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(doMC)

# handle common conflicts
tidymodels_prefer()

# best model: 

# load tuned and training data
load(here("data/tuned_models/_tuned.rda"))
load(here("data/model_data/diabetes_train.rda"))

# finalize workflow ----
final_wflow <- _tuned |> 
  extract_workflow(_tuned) |>  
  finalize_workflow(select_best(_tuned, metric = "accuracy"))

# train final model ----
# set seed
set.seed(34522)
final_fit <- fit(final_wflow, diabetes_train)

# write out fitted data
save(final_fit, file = here("results/final_model/_final_fit.rda"))

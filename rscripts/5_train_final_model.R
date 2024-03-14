# Final Project II ----
# Train final model
# Best Model: kitchen sink Boosted Trees (currently)
# BE AWARE: there is a random process in this script (seed set right before it)

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)

# handle common conflicts
tidymodels_prefer()

# best model: kitchen sink Boosted Trees 

# load tuned and training data
load(here("data/tuned_models/bt_tuned.rda"))
load(here("data/model_data/diabetes_train.rda"))

# finalize workflow ----
final_wflow <- bt_tuned |> 
  extract_workflow(bt_tuned) |>  
  finalize_workflow(select_best(bt_tuned, metric = "accuracy"))

# train final model ----
# set seed
set.seed(34522)
final_fit <- fit(final_wflow, diabetes_train)

# write out fitted data
save(final_fit, file = here("results/final_model/bt_final_fit.rda"))
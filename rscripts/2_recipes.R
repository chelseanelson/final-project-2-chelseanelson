# L06 Model Tuning ----
# Setup pre-processing/recipes

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)

# handle common conflicts
tidymodels_prefer()

## loading in training data 
load(here("data/model_data/diabetes_train.rda"))

# build recipes

# recipe 1 (baseline) ----

## variation 1 
baseline_rec <- recipe(diabetes_binary ~ ., 
                       data = diabetes_train) %>% 
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_center(all_numeric_predictors()) %>% 
  step_scale(all_numeric_predictors())

# variation 2 
baseline_naive_bayes_rec <-
  recipe(diabetes_binary ~ ., 
         data = diabetes_train) %>% 
  step_zv(all_predictors()) %>%
  step_center(all_numeric_predictors()) %>% 
  step_scale(all_numeric_predictors())

# check recipe 
baseline_rec %>%
  prep() %>%
  bake(new_data = NULL) %>%
  glimpse()

baseline_naive_bayes_rec %>% 
  prep() %>%
  bake(new_data = NULL) %>%
  glimpse()

# recipe 2 (tree-based and k-nearest neighbors) ----

# expand on these

diabetes_rec_treebased <- recipe(diabetes_binary ~ ., 
                       data = diabetes_train) %>% 
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_center(all_predictors()) %>% 
  step_scale(all_predictors())

diabetes_rec_knn <- recipe(diabetes_binary ~ ., 
                           data = diabetes_train) %>% 
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_center(all_predictors()) %>% 
  step_scale(all_predictors()) 

# recipe 3 (neural networks) ----


# write out recipe(s) 
save(baseline_rec, file = here("data/recipes/baseline_rec.rda"))
save(baseline_naive_bayes_rec, file = here("data/recipes/baseline_naive_bayes_rec.rda"))
save(diabetes_rec_treebased, file = here("data/recipes/diabetes_rec_treebased.rda"))
save(diabetes_rec_knn, file = here("data/recipes/diabetes_rec_knn.rda"))

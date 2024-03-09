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

## variation 1 (parametric)
baseline_rec <- recipe(diabetes_binary ~ ., 
                       data = diabetes_train) %>% 
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_center(all_numeric_predictors()) %>% 
  step_scale(all_numeric_predictors())

## variation 2 (naive bayes)
baseline_naive_bayes_rec <-
  recipe(diabetes_binary ~ ., 
         data = diabetes_train) %>% 
  step_zv(all_predictors()) %>%
  step_center(all_numeric_predictors()) %>% 
  step_scale(all_numeric_predictors())

## variation 3 (tree-based)

baseline_tree_rec <- recipe(diabetes_binary ~ ., 
                       data = diabetes_train) %>% 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
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

baseline_tree_rec %>% 
  prep() %>%
  bake(new_data = NULL) %>%
  glimpse()

# recipe 2 (feature engineering) ---- # work on this one still 

## variation 1 

## variation 1 (parametric)
feature_eng_rec <- recipe(diabetes_binary ~ ., 
                       data = diabetes_train) %>% 
  step_rm(any_healthcare, chol_check, no_docbc_cost) %>%
  step_dummy(all_nominal_predictors()) %>%
  # add interactions here 
  # Yes, step_interact() in a recipe in R can work with two factors (categorical variables). It creates interaction terms between the levels of the specified factors. Note that if your factors have many levels this may create too many predictor variables!
  # add transformations to handle skewness and class imbalance
  step_zv(all_predictors()) %>%
  step_center(all_numeric_predictors()) %>% 
  step_scale(all_numeric_predictors())

## variation 2 (tree-based)

feature_eng_tree_rec <- recipe(diabetes_binary ~ ., 
                            data = diabetes_train) %>% 
  step_rm(any_healthcare, chol_check, no_docbc_cost) %>% 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  # add decorrelation here
  # add transformations to handle skewness and class imbalance
  step_zv(all_predictors()) %>%
  step_center(all_numeric_predictors()) %>% 
  step_scale(all_numeric_predictors())

# check recipe 
feature_eng_rec %>%
  prep() %>%
  bake(new_data = NULL) %>%
  glimpse()

feature_eng_rec %>% 
  prep() %>%
  bake(new_data = NULL) %>%
  glimpse()

# write out recipe(s) 
save(baseline_rec, file = here("data/recipes/baseline_rec.rda"))
save(baseline_naive_bayes_rec, file = here("data/recipes/baseline_naive_bayes_rec.rda"))
save(baseline_tree_rec, file = here("data/recipes/baseline_tree_rec.rda"))
save(feature_eng_rec, file = here("data/recipes/feature_eng_rec.rda"))
save(feature_eng_tree_rec, file = here("data/recipes/feature_eng_tree_rec.rda"))


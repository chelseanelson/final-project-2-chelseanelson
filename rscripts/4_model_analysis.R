# Final Project II ----
# Analysis of tuned and trained models (comparisons)
# Main Assessment Metric: Accuracy

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)

# handle common conflicts
tidymodels_prefer()

# loading in tuned models 
load(here("data/fitted_models/fit_folds_logistic.rda"))
load(here("data/fitted_models/fit_folds_nbayes.rda"))

# comparing sub-models ----
## KNN
knn_plot <- knn_tuned %>% autoplot(metric = "rmse")
knn_best <- select_best(knn_tuned, metric = "rmse") %>% select(-.config)

# Random Forest
rf_plot <- rf_tuned %>% autoplot(metric = "rmse")
rf_best <- select_best(rf_tuned, metric = "rmse") %>% select(-.config)

# Boosted Tree
bt_plot <- bt_tuned %>% autoplot(metric = "rmse")
bt_best <- select_best(bt_tuned, metric = "rmse") %>% select(-.config)



model_results <- as_workflow_set(knn = knn_tuned, rf = rf_tuned, bt = bt_tuned)

comparison_rmse_model <- model_results %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  slice_min(mean, by = wflow_id) %>% 
  arrange(mean) %>%
  select(wflow_id, mean, std_err, n) 
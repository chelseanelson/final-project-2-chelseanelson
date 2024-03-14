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
load(here("data/fitted_models/fit_folds_logistic_2.rda"))
load(here("data/tuned_models/nnet_tuned.rda"))
load(here("data/tuned_models/knn_tuned.rda"))
load(here("data/tuned_models/nnet_tuned_2.rda"))
load(here("data/tuned_models/knn_tuned_2.rda"))
load(here("data/tuned_models/rf_tuned.rda"))
load(here("data/tuned_models/bt_tuned.rda"))
load(here("data/tuned_models/bt_tuned_2.rda"))
load(here("data/tuned_models/rf_tuned_2.rda"))

# comparing sub-models ----
## Logistic Regressions
logistic_best_1 <- show_best(fit_folds_logistic, metric = "accuracy")
logistic_best_2 <- show_best(fit_folds_logistic_2, metric = "accuracy")

## Naive Bayes 
nbayes_best <- show_best(fit_folds_nbayes, metric = "accuracy")

## Neutral Networks
nnet_best_1 <- show_best(nnet_tuned, metric = "accuracy")
nnet_best_2 <- show_best(nnet_tuned_2, metric = "accuracy")

## K-Nearest Neighbors 
knn_best_1 <- show_best(knn_tuned, metric = "accuracy")
knn_best_2 <- show_best(knn_tuned_2, metric = "accuracy")

## Boosted Trees
bt_best_1 <- show_best(bt_tuned, metric = "accuracy")
bt_best_2 <- show_best(bt_tuned_2, metric = "accuracy")

## Random Forests 
rf_best_1 <- show_best(rf_tuned, metric = "accuracy")
rf_best_2 <- show_best(rf_tuned_2, metric = "accuracy")

model_results_nbayes_logistic <- as_workflow_set(nbayes = fit_folds_nbayes,
                                                 logisitic = fit_folds_logistic)

model_results <- as_workflow_set(nbayes = fit_folds_nbayes,
                                 logisitic_1 = fit_folds_logistic,
                                 logistic_2 = fit_folds_logistic_2,
                                 nnet_1 = nnet_tuned,
                                 nnet_2 = nnet_tuned_2,
                                 knn_1 = knn_tuned,
                                 knn_2 = knn_tuned_2,
                                 bt_1 = bt_tuned,
                                 bt_2 = bt_tuned_2,
                                 rf_1 = rf_tuned,
                                 rf_2 = rf_tuned_2
)

nbayes_logistic_accuracy_model <- model_results_nbayes_logistic %>%
  collect_metrics() %>%
  filter(.metric == "accuracy") %>%
  slice_max(mean, by = wflow_id) %>% 
  arrange(mean) %>%
  select(wflow_id, .metric, mean, std_err, n) %>%
  rename(metric = .metric)

model_accuracy_comparison <- model_results %>%
  collect_metrics() %>%
  filter(.metric == "accuracy") %>%
  slice_max(mean, by = wflow_id) %>% 
  arrange(std_err) %>%
  arrange(desc(mean)) %>% 
  select(wflow_id, .metric, mean, std_err, n) %>% 
  rename(metric = .metric)

# write out results (plots, tables)
write_rds(nbayes_logistic_accuracy_model, file = here("results/tuned_models/nbayes_logistic_accuracy_model.rds"))
write_rds(model_accuracy_comparison, file = here("results/tuned_models/model_accuracy_comparison.rds"))

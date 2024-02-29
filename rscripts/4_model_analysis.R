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
## Logistic Regression
logistic_best <- select_best(fit_folds_logistic, metric = "accuracy") 

## Naive Bayes 
nbayes_best <- select_best(fit_folds_nbayes, metric = "accuracy")

model_results <- as_workflow_set(logisitic = fit_folds_logistic, nbayes = fit_folds_nbayes)

nbayes_logistic_accuracy_model <- model_results %>%
  collect_metrics() %>%
  filter(.metric == "accuracy") %>%
  slice_min(mean, by = wflow_id) %>% 
  arrange(mean) %>%
  select(wflow_id, .metric, mean, std_err, n) %>%
  rename(metric = .metric)

# write out results (plots, tables)
write_rds(nbayes_logistic_accuracy_model, file = here("results/tuned_models/nbayes_logistic_accuracy_model.rds"))

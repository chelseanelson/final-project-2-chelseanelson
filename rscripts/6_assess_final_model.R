# Final Project II ----
# Assess final model

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)

# handle common conflicts
tidymodels_prefer()

# load testing and fitted data
load(here("data/model_data/diabetes_test.rda"))
load(here("results/final_model/bt_final_fit.rda"))

# assessing models performance 
prediction_metrics <- metric_set(accuracy, mcc)

diabetes_test_res <- bind_cols(diabetes_test, predict(final_fit, diabetes_test)) %>%
  select(diabetes_binary, .pred_class)

diabetes_model_metrics <- prediction_metrics(diabetes_test_res, truth = diabetes_binary, estimate = .pred_class)

performance_table <- 
  tibble(
    metrics = c("Accuracy", "MCC"),
    estimate = diabetes_model_metrics %>% pull(.estimate)
  )

confusion_matrix <- 
  conf_mat(diabetes_test_res, truth = diabetes_binary, estimate = .pred_class)

heatmap <- autoplot(confusion_matrix, type = "heatmap") +
  labs(title = "Confusion Matrix Heatmap",
       x = "Predicted Diabetes Diagnosis",
       y = "True Diabetes Diagnosis") +
  theme_minimal() +
  scale_fill_gradient(low = "gray", high = "purple") +
  theme(legend.position = "none")

# save out results (plot, table)
write_rds(performance_table, here("results/final_model/performance_table.rds"))
write_rds(confusion_matrix, here("results/final_model/confusion_matrix.rds"))
ggsave(here("figures/final_model/heatmap.png"), heatmap)

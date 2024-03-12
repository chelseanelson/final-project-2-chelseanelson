# Final Project II ----
# Initial data checks & data splitting
# BE AWARE: there are random processes in this script (seed set right before them)

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)

# handle common conflicts
tidymodels_prefer()

# loading datasets
diabetes_data_large <- read_csv(here("data/raw/diabetes_012_health_indicators_BRFSS2015.csv")) %>% 
  janitor::clean_names() 
diabetes_data_small <- read_csv(here("data/raw/diabetes_binary_5050split_health_indicators_BRFSS2015.csv")) %>%
  janitor::clean_names()

# skimming the data
diabetes_data_large %>% skimr::skim_without_charts()
diabetes_data_small %>% skimr::skim_without_charts()

# early manipulation of data 
diabetes_data_large <- diabetes_data_large %>%
  mutate(
    diabetes_012 = factor(diabetes_012, labels = c("no diabetes", "prediabetes", "diabetes"))
  )

diabetes_data_small <- diabetes_data_small %>%
  mutate(
    diabetes_binary = factor(diabetes_binary)
  )

# looking at target variable 
diabetes_univariate_large <- diabetes_data_large %>% ggplot(aes(diabetes_012)) + 
  geom_bar(stat = "count") + 
  labs(title = "Distribution of Diabetes Diagnoses", x = "Diabetes Diagnoses",
       y = "Count") + geom_text(stat = "count", aes(label = ..count..),
                                vjust = -0.5) + 
  theme_minimal() 

diabetes_univariate_small <- 
  diabetes_data_small %>% ggplot(aes(diabetes_binary)) + 
  geom_bar(stat = "count") +
  labs(title = "Distribution of Diabetes Diagnoses", x = "Diabetes Diagnoses",
       y = "Count") + geom_text(stat = "count", aes(label = ..count..),
            vjust = -0.5) + 
  scale_x_discrete(labels = c("no diabetes", "diabetic")) + 
  theme_minimal() 

# saving figures 
ggsave(here("figures/figure-1.png"), diabetes_univariate_large)
ggsave(here("figures/figure-2.png"), diabetes_univariate_small)

# I have decided to go with the larger dataset, as it will be better suited for 
# the objectives that I wish to complete, and then I will be taking a 
# random sample of the large group (no diabetes) and combine it with the 
# small group (pre-diabetes and diabetes).

# According to an ADA expert panel, up to 70% of individuals with prediabetes 
# will eventually develop diabetes. 

# additional manipulations
diabetes_data_large <- diabetes_data_large %>%
  mutate(
    diabetes_012 = fct_collapse(diabetes_012, 
                                diabetes = c("prediabetes", "diabetes")),
    high_bp = factor(high_bp),
    high_chol = factor(high_chol),
    chol_check = factor(chol_check),
    smoker = factor(smoker),
    stroke = factor(stroke),
    heart_diseaseor_attack = factor(heart_diseaseor_attack),
    phys_activity = factor(phys_activity),
    fruits = factor(fruits),
    veggies = factor(veggies),
    hvy_alcohol_consump = factor(hvy_alcohol_consump),
    any_healthcare = factor(any_healthcare),
    no_docbc_cost = factor(no_docbc_cost),
    gen_hlth = factor(gen_hlth, labels = c("excellent", "very good", "good", "fair", "poor")),
    diff_walk = factor(diff_walk),
    sex = factor(sex, labels = c("female", "male"))
  ) %>%
  rename(
    diabetes_binary = diabetes_012
  )

# creating diabetes only dataset
diabetes_data_diabetes_level <- diabetes_data_large %>% filter(diabetes_binary == "diabetes") 

# creating none diabetic only dataset
# set seed for random sample
set.seed(1234)

diabetes_data_no_diabetes_level <- diabetes_data_large %>% filter(diabetes_binary == "no diabetes") %>%
  slice_sample(n = 39977)

# creating the full dataset now 
diabetes_data <- bind_rows(diabetes_data_no_diabetes_level, diabetes_data_diabetes_level) 

# checking the data 
diabetes_data %>% skimr::skim_without_charts()

# There is no missing data in this dataset !!

# checking the response variable 
diabetes_data_response_var <- diabetes_data %>% ggplot(aes(diabetes_binary)) + 
  geom_bar() +
  labs(title = "Distribution of Diabetes Diagnoses", x = "Diabetes Diagnoses",
       y = "Count") + geom_text(stat = "count", aes(label = ..count..),
                                vjust = -0.5) + theme_minimal() 

ggsave(diabetes_data_response_var, file = here("figures/figure-3.png"))

# writing out updated data
write_rds(diabetes_data, here("data/diabetes_data.rds")) 

## set seed for random split 
set.seed(61731)

# initial split of the data 
diabetes_splits <- diabetes_data %>% 
  initial_split(prop = 0.75, strata = diabetes_binary)

diabetes_train <- diabetes_splits %>% training()
diabetes_test <- diabetes_splits %>% testing()

dim(diabetes_train)
dim(diabetes_test)

# folding data (resamples) ----
# set seed
set.seed(123451)
diabetes_folds <- vfold_cv(diabetes_train, v = 10, repeats = 5, strata = diabetes_binary)

# set up controls for fitting resamples
keep_pred <- control_resamples(save_pred = TRUE, save_workflow = TRUE)

# write out split, train, test and folds 
save(diabetes_splits, file = here("data/model_data/diabetes_splits.rda"))
save(diabetes_train, file = here("data/model_data/diabetes_train.rda"))
save(diabetes_test, file = here("data/model_data/diabetes_test.rda"))
save(diabetes_folds, file = here("data/model_data/diabetes_folds.rda"))
save(keep_pred, file = here("results/keep_preds.rda"))




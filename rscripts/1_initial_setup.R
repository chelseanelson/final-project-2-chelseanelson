# Final Project II ----
# Initial data checks & data splitting
# BE AWARE: there is a random process in this script (seed set right before it)

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)

# handle common conflicts
tidymodels_prefer()

# loading datasets
diabetes_data_large <- read_csv(here("data/diabetes_012_health_indicators_BRFSS2015.csv")) %>% 
  janitor::clean_names()
diabetes_data_small <- read_csv(here("data/diabetes_binary_5050split_health_indicators_BRFSS2015.csv")) %>% 
  janitor::clean_names()

# skimming the data
diabetes_data_large %>% skimr::skim_without_charts()
diabetes_data_small %>% skimr::skim_without_charts()

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

# manipulating data 
diabetes_data_large <- diabetes_data_large %>%
  mutate(
    diabetes_012 = factor(diabetes_012)
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
  scale_x_discrete(labels = c("no diabetes", "prediabetic", "diabetic")) + 
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




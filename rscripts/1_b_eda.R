# Final Project II ----
# Exploratory Data Analysis on subsection of training data 
# BE AWARE: there are random processes in this script (seed set right before them)

# load packages ----
library(tidyverse)
library(tidymodels)
library(here)

# handle common conflicts
tidymodels_prefer()

# load traning data 
load(here("data/model_data/diabetes_train.rda"))

set.seed(3463452)
diabetes_eda_data <- diabetes_train %>% slice_sample(prop = .8)

diabetes_eda_data %>% skimr::skim_without_charts()

# univariate analysis ----

## continuous variables ---- 

univariate_continuous <- function(df, numerical_var, filename) {
  
 g <- df %>% 
    ggplot(aes( {{ numerical_var }} )) + 
    geom_histogram(color = "white") + 
    labs(
      title = paste("Distribution of", quo_name(ensym(numerical_var))),
      y = "Count"
    ) + 
    theme_minimal()
 
 ggsave(here(paste0("figures/univariate-EDA/" ,filename, ".png")), g)
}

diabetes_eda_data %>% univariate_continuous(bmi, "numerical_univariate_1")
# this is extremely left skewed, thus we might want to perform a log10 transformation on this

diabetes_eda_data %>% univariate_continuous(ment_hlth, "numerical_univariate_2")
# This data is very sparse, thus we might want to put fillers in this
# Do not add log10 to this plot, it made it all NaN

diabetes_eda_data %>% univariate_continuous(phys_hlth, "numerical_univariate_3")
# The data is also very sparse, thus we might want to move them more to the middle or creating a more
# normal distribution somehow 
# Do not add log10 to this plot, it made it all NaN

diabetes_eda_data %>% univariate_continuous(age, "numerical_univariate_4")
# The age variable is somewhat right skewed thus we might want to perform a log10 transformation on this

diabetes_eda_data %>% univariate_continuous(education, "numerical_univariate_5")
# The data is somewhat right skewed thus we might want to perform a log10 transformation on this 

diabetes_eda_data %>% univariate_continuous(income, "numerical_univariate_6")
# The data is again somewhat right skewed thus we might want to perform a log10 transformation on this

# log10 transformation of bmi
numerical_univariate_7 <- diabetes_eda_data %>% ggplot(aes(bmi)) + geom_histogram() + scale_x_continuous(trans = "log10") + labs(title = "Distribution of bmi (log-10)", y = "Count") + theme_minimal()

ggsave(here("figures/univariate-EDA/numerical_univariate_7.png"), numerical_univariate_7)

## discrete variables ----

univariate_discrete <- function(df, categorical_var) {
  
  df %>% 
    ggplot(aes( {{ categorical_var }} )) + 
    geom_bar(color = "white") + 
    labs(
      title = paste("Distribution of",
        str_replace_all(quo_name(ensym(categorical_var)), "_", " ")),
      y = "Count"
    ) + 
    theme_minimal()
}

categorical_univariate_1 <- diabetes_eda_data %>% univariate_discrete(chol_check) + 
  scale_x_discrete(labels = c("0" = "no cholesterol check in 5 years", 
                              "1" = " yes cholesterol check in 5 years"))
# this one has high class imbalance 

categorical_univariate_2 <- diabetes_eda_data %>% univariate_discrete(stroke) + 
  scale_x_discrete(labels = c("0" = "never had a stroke", 
                              "1" = "yes had a stroke"))
# high class imbalance 

categorical_univariate_3 <- diabetes_eda_data %>% univariate_discrete(heart_diseaseor_attack) + 
  scale_x_discrete(labels = c("0" = "never had heart diseases", 
                              "1" = "yes have had heart diseases"))
# high class imbalance 

categorical_univariate_4 <- diabetes_eda_data %>% univariate_discrete(phys_activity)  + 
  scale_x_discrete(labels = c("0" = "no physical activity in 30 days", 
                              "1" = "yes physical activity in 30 days"))
# high class imbalance 

categorical_univariate_5 <- diabetes_eda_data %>% univariate_discrete(fruits) + 
  scale_x_discrete(labels = c("0" = "don't consume fruit 1 or more times per day", 
                              "1" = "consumes fruit 1 or more times per day"))
# high class imbalance 

categorical_univariate_6 <- diabetes_eda_data %>% univariate_discrete(veggies) + 
  scale_x_discrete(labels = c("0" = "don't consume veggies 1 or more times per day", 
                              "1" = "consumes veggies 1 or more times per day"))
# high class imbalance 

categorical_univariate_7 <- diabetes_eda_data %>% univariate_discrete(hvy_alcohol_consump) + 
  scale_x_discrete(labels = c("0" = "not a heavy drinker", 
                              "1" = "a heavy drinker"))
  
# high class imbalance 

categorical_univariate_8 <- diabetes_eda_data %>% univariate_discrete(any_healthcare) + 
  scale_x_discrete(labels = c("0" = "doesn't have healthcare", 
                              "1" = "has healthcare"))
  
# high class imbalance 

categorical_univariate_9 <- diabetes_eda_data %>% univariate_discrete(no_docbc_cost) + 
  scale_x_discrete(labels = c("0" = "didn't need to see doctor",
                              "1" = "needed to see doctor but couldn't pay"))
# high class imbalance 

categorical_univariate_10 <- diabetes_eda_data %>% univariate_discrete(gen_hlth)
# high class imbalance but normal distrubtion 

categorical_univariate_11 <- diabetes_eda_data %>% univariate_discrete(diff_walk) + 
  scale_x_discrete(labels = c("0" = "no difficultly walking up stairs", 
                              "1" = "difficultly walking up stairs"))
# high class imbalance 

categorical_univariate_12 <- diabetes_eda_data %>% univariate_discrete(high_bp) + 
  scale_x_discrete(labels = c("0" = "no high BP", 
                              "1" = "high BP"))
# low class imbalance 

categorical_univariate_13 <- diabetes_eda_data %>% univariate_discrete(high_chol) +
  scale_x_discrete(labels = c("0" = "no high cholesterol", 
                              "1" = "high cholesterol"))
# low class imbalance 

categorical_univariate_14 <- diabetes_eda_data %>% univariate_discrete(smoker) +
  scale_x_discrete(labels = c("0" = "had not smoked at least 100 cigarettes", 
                              "1" = "smoked at least 100 cigarettes"))
# low class imbalance 

categorical_univariate_15 <- diabetes_eda_data %>% univariate_discrete(sex)
# low class imbalance 

ggsave("figures/univariate-EDA/categorical_univariate_1.png", categorical_univariate_1)
ggsave("figures/univariate-EDA/categorical_univariate_2.png", categorical_univariate_2)
ggsave("figures/univariate-EDA/categorical_univariate_3.png", categorical_univariate_3)
ggsave("figures/univariate-EDA/categorical_univariate_4.png", categorical_univariate_4)
ggsave("figures/univariate-EDA/categorical_univariate_5.png", categorical_univariate_5)
ggsave("figures/univariate-EDA/categorical_univariate_6.png", categorical_univariate_6)
ggsave("figures/univariate-EDA/categorical_univariate_7.png", categorical_univariate_7)
ggsave("figures/univariate-EDA/categorical_univariate_8.png", categorical_univariate_8)
ggsave("figures/univariate-EDA/categorical_univariate_9.png", categorical_univariate_9)
ggsave("figures/univariate-EDA/categorical_univariate_10.png", categorical_univariate_10)
ggsave("figures/univariate-EDA/categorical_univariate_11.png", categorical_univariate_11)
ggsave("figures/univariate-EDA/categorical_univariate_12.png", categorical_univariate_12)
ggsave("figures/univariate-EDA/categorical_univariate_13.png", categorical_univariate_13)
ggsave("figures/univariate-EDA/categorical_univariate_14.png", categorical_univariate_14)
ggsave("figures/univariate-EDA/categorical_univariate_15.png", categorical_univariate_15)

# bivariate analysis ----

# BP and Chol
diabetes_eda_data %>% 
  ggplot(aes(x = high_bp, fill = high_chol)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Blood Pressure by Cholsterol Levels") +
  scale_x_discrete(labels = c("0" = "no high BP", 
                              "1" = "high BP")) +
  scale_fill_discrete(labels = c("0" = "no high cholesterol", 
                                 "1" = "high cholesterol")) + 
  theme_minimal()

# These are seem very correlated to each other, might be good to look at the
# interactions between them later on 

# Veggies and Fruits 
diabetes_eda_data %>% 
  ggplot(aes(x = veggies, fill = fruits)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Blood Pressure by Cholsterol Levels") +
  scale_fill_discrete(labels = c("0" = "don't consume fruit 1 or more times per day", 
                              "1" = "consumes fruit 1 or more times per day")) +
  scale_x_discrete(labels = c("0" = "don't consume veggies 1 or more times per day", 
                                 "1" = "consumes veggies 1 or more times per day")) + 
  theme_minimal()

# These seem to have a somewhat strong relationship between them

# want to look at BMI, Chol, BP relationships 

# BMI and Blood Pressure
diabetes_eda_data %>%
  ggplot(aes(bmi, high_bp)) + 
  geom_boxplot() + scale_y_discrete(labels = c("0" = "no high BP", 
                                               "1" = "high BP")) + 
  theme_minimal() + labs(title = "BMI by Blood Pressure")

# no relationship here 

# BMI and Cholesterol
diabetes_eda_data %>%
  ggplot(aes(bmi, high_chol)) + 
  geom_boxplot() + scale_y_discrete(labels = c("0" = "no high cholesterol", 
                                               "1" = "high cholesterol")) + 
  theme_minimal() + labs(title = "BMI by Cholesterol Levels")

# interesting that there is no relationship between high cholesterol and bmi, as well as 
# bmi and high blood pressure, however we do see that the median for having high cholesterol or 
# high blood pressure is more than not having 

# General Health and High Cholesterol
diabetes_eda_data %>% 
  ggplot(aes(x = gen_hlth, fill = high_chol)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "General Health by Cholsterol Levels") +
  scale_fill_discrete(labels = c("0" = "no high cholesterol", 
                                 "1" = "high cholesterol")) + 
  theme_minimal()

# There is a large correlation between these two variables, as general health
# assessment gets worse, the number of people having high cholesterol increases
# higher past the people who don't have high cholesterol 

diabetes_eda_data %>% 
  ggplot(aes(x = gen_hlth, fill = high_bp)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "General Health by Cholsterol Levels") +
  scale_fill_discrete(labels = c("0" = "no high BP", 
                                 "1" = "high BP")) + 
  theme_minimal()

# There is a large correlation between these two variables, as general health
# assessment gets worse, the number of people having high blood pressure increases
# higher past the people who don't have high blood pressure 

# BMI and General Health
diabetes_eda_data %>%
  ggplot(aes(gen_hlth, bmi)) + 
  geom_boxplot() + theme_minimal() + 
  labs(title = "BMI by General Health")

# I would not say that they are completely correlated however we do see 
# through the plot that has the general health assessment score does 
# get lower, the median rises in terms of BMI. However most of the overall
# distributions look the same for each general health assessment, being left
# skewed with a few outliers 

# General Health and Difficulty walking up stairs 
diabetes_eda_data %>% 
  ggplot(aes(x = gen_hlth, fill = diff_walk)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Blood Pressure by Cholsterol Levels") +
  scale_fill_discrete(labels = c("0" = "no difficultly walking up stairs", 
                                 "1" = "difficultly walking up stairs")) + 
  theme_minimal()

# There seems to be an interesting relationship between these two variables

# General Health and Physical Activity 
diabetes_eda_data %>% 
  ggplot(aes(x = gen_hlth, fill = phys_activity)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "General Health by Physical Activity") +
  scale_fill_discrete(labels = c("0" = "no physical activity in 30 days", 
                                 "1" = "yes physical activity in 30 days")) + 
  theme_minimal()

# Not that strong of a relationship has no physical activity in 30 days 
# seems to outweigh having physical activity in 30 days only for the poor health
# assessment score. Although the rise of no phyisical activity is constant 
# from better to worse general health.

# A lot of other variables like to interact with general health 

# BP and Diabetes
diabetes_eda_data %>% 
  ggplot(aes(x = high_bp, fill = diabetes_binary)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Blood Pressure by Diabetes") +
  scale_x_discrete(labels = c("0" = "no high BP", 
                              "1" = "high BP")) +
  theme_minimal()

# This is pretty correlated with diabetes 

diabetes_eda_data %>% 
  ggplot(aes(x = high_chol, fill = diabetes_binary)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Cholsterol Levels by Diabetes") +
  scale_x_discrete(labels = c("0" = "no high cholsterol", 
                              "1" = "high cholsterol")) +
  theme_minimal()

# This is also pretty correlated to diabetes 

diabetes_eda_data %>% 
  ggplot(aes(x = gen_hlth, fill = diabetes_binary)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "General Health by Diabetes") +
  theme_minimal()

# This is also pretty correlated to diabetes 

diabetes_eda_data %>%
  ggplot(aes(diabetes_binary, bmi)) + 
  geom_boxplot() + theme_minimal() + 
  labs(title = "BMI by Diabetes")

# The average bmi is higher for people with diabetes although the distributions
# overall are quite similar 

# Not that strong of a relationship has no physical activity in 30 days 
# seems to outweigh having physical activity in 30 days only for the poor health
# assessment score. Although the rise of no phyisical activity is constant 
# from better to worse general health.

# A lot of other variables like to interact with general health 

# healthcare, cholesterol check and doctor variables might not actually matter,
# take these out in the feature engineering recipes 

### write out plots and eda_data  
# write out the plots associated with everything i looked at, and then within my 
# eda I can highlight some and then just showcase the others 



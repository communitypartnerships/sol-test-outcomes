## Purpose of script: to simulate the missing MATH data and then model for all VA students.

# Note: because the data sets are large, running the models all at once can crash RStudio. 
# This script saves and exports each model so we can clear the RSession each time. 
# Recommend removing large df from environment after completing each demographic group (see script). 
# Future versions could modularize this process with a function and DRY programming style. 

# Libraries ----
library(here)
library(tidyverse)
library(lme4)
library(stringr)

# Set WD ----
setwd(here("shinyapps/dataprep"))

# Create temp data folder
if (!dir.exists("tempdata")) {
  # If it does not exist, create the directory
  dir.create("tempdata", recursive = TRUE)
  message(paste("Directory created:", "tempdata"))
} else {
  message(paste("Directory already exists:", "tempdata"))
}

# Get data ----
complete_sol <- read_csv("complete_sol_data.csv")

# Filter for Math
math <- complete_sol %>% filter(subject == "Mathematics")

# Remove 7th and 8th Grade Math
math <- math %>% filter(grade < 7)

# Get divisions
division_list <- math %>%
  select(division_name) %>%
  unique()

# Model ----
# Define model:
yearly_model <- function(df) {
  glmer(pass ~   label + 
          (1 + label|division_name),
        family = binomial("logit"), 
        data = df)
}

## Race ----

# Split into subgroups: Race
math_race <- math %>%
  filter(group == "Race")

# Explore missing data:
# In theory, the random coefficients helps draw those measures towards the center 
# so that helps. But at the same time, a bit precarious. 
math_race %>%
  mutate(missing = case_when(
    is.na(pass_count) ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(test_year, label) %>%
  summarize(missing = mean(missing)) %>%
  spread(test_year, missing)

math_race %>%
  filter(is.na(pass_count)) %>%
  pull(division_name) %>%
  unique()

# If the numerator equals zero/is not measured then set the denominator to 0 as well
# to not be measured (make the data point missing as opposed to having no one pass the exam)
math_race[is.na(math_race)] <- 0 
math_race$total_count[math_race$pass_count == 0] <- 0  

# Simulate missing student data:
math_race <- math_race %>%
  uncount(total_count) %>%
  group_by(division_name, label, grade, pass_rate, test_year, pass_count) %>%
  mutate(id = 1:n(),
         pass = case_when(
           id <= pass_count ~ 1,
           TRUE ~ 0))

# Create nested data frame with only columns relevant to the model (one df per year):
math_race_byyear <- math_race %>% 
  select(label, division_name, test_year, pass) %>%
  group_by(test_year) %>% 
  nest()

# Estimate models: takes a lot of RAM 
math_race_byyear <- math_race_byyear %>% 
  mutate(model = map(data, yearly_model))

# Intermediary step if need to return to modeled data at later point (ie RStudio crash)
# Save RDS file
saveRDS(math_race_byyear, "tempdata/math_race_byyear.RDS" )
# Read in previously saved RDS file
# math_race_byyear <- readRDS("tempdata/math_race_byyear.RDS")

# Get model coefficients:
math_race_coefs <- math_race_byyear %>%
  mutate(
    coefficients = map(model, ~coef(.x)$division_name %>%
                         data.frame() %>%
                         rownames_to_column() %>%
                         tibble() %>%
                         rename(
                           division_name = 1,
                           intercept = 2,
                           label = 3
                         ) %>%
                         mutate(
                           black = exp(intercept)/(1 + exp(intercept)),
                           white = exp(intercept + label)/(1 + exp(intercept + label))
                         )
                       )
  ) %>%
  select(-data, -model) %>%
  unnest(coefficients)

# Format: 
math_race_coefs <- math_race_coefs %>%
  select(test_year, division_name, black, white) %>%
  gather(
    label, rate, -test_year, -division_name
  ) %>%
  mutate(group = "Race")
  
# Save:
write_csv(math_race_coefs, "model_data/math_race.csv")

# Remove large df from environment
rm(math_race_byyear)
 
## Economic Status ----

# Split into subgroups:Economic Status
math_disadvan <- math %>%
  filter(group == "Economic Status")

# Explore missing data:
math_disadvan %>%
  mutate(missing = case_when(
    is.na(pass_count) ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(test_year, label) %>%
  summarize(missing = mean(missing)) %>%
  spread(test_year, missing)

math_disadvan %>%
  filter(is.na(pass_count)) %>%
  pull(division_name) %>%
  unique()

# If the numerator equals zero/is not measured then set the denominator to 0 as well
# to not be measured (make the data point missing as opposed to having no one pass the exam)
math_disadvan[is.na(math_disadvan)] <- 0 
math_disadvan$total_count[math_disadvan$pass_count == 0] <- 0  

# Simulate missing student data:
math_disadvan <- math_disadvan %>%
  uncount(total_count) %>%
  group_by(division_name, label, grade, pass_rate, test_year, pass_count) %>%
  mutate(id = 1:n(),
         pass = case_when(
           id <= pass_count ~ 1,
           TRUE ~ 0)) 

# Create nested data frame with only columns relevant to the model (one df per year):
math_disadvan_byyear <- math_disadvan %>% 
  select(label, division_name, test_year, pass) %>%
  group_by(test_year) %>% 
  nest()
 
# Estimate models: takes time
math_disadvan_byyear <- math_disadvan_byyear %>% 
  mutate(model = map(data, yearly_model))

# Intermediary step if need to return to modeled data at later point (ie RStudio crash)
# Save RDS file
saveRDS(math_disadvan_byyear, "tempdata/math_disadvan_byyear.RDS")
# Read in previously saved RDS file
# math_disadvan_byyear <- readRDS("tempdata/math_disadvan_byyear.RDS")

# Get coefficients:
math_disadvan_coefs <- math_disadvan_byyear%>%
  mutate(
    coefficients = map(model, ~coef(.x)$division_name %>%
                         data.frame() %>%
                         rownames_to_column() %>%
                         tibble() %>%
                         rename(
                           division_name = 1,
                           intercept = 2,
                           label = 3
                         ) %>%
                         mutate(
                           disadvantaged = exp(intercept)/(1 + exp(intercept)),
                           non_disadvantaged = exp(intercept + label)/(1 + exp(intercept + label))
                         )
    )
  ) %>%
  select(-data, -model) %>%
  unnest(coefficients)

# Format:
math_disadvan_coefs <- math_disadvan_coefs %>%
  select(test_year, division_name, disadvantaged, non_disadvantaged) %>%
  gather(
    label, rate, -test_year, -division_name
  ) %>%
  mutate(group = "Economic Status")

# Save:
write_csv(math_disadvan_coefs, "model_data/math_disadvan.csv")

# Remove large df from environment
rm(math_disadvan_byyear)

## English Learner Status ----

# Split into subgroups: English Learner Status 
math_englearner <- math %>%
  filter(group == "English Learner Status")

# Explore missing values:
math_englearner %>%
  mutate(missing = case_when(
    is.na(pass_count) ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(test_year, label) %>%
  summarize(missing = mean(missing)) %>%
  spread(test_year, missing)

math_englearner %>%
  filter(is.na(pass_count)) %>%
  pull(division_name) %>%
  unique()

# If the numerator equals zero/is not measured then set the denominator to 0 as well
# to not be measured (make the data point missing as opposed to having no one pass the exam)
math_englearner[is.na(math_englearner)] <- 0
math_englearner$total_count[math_englearner$pass_count == 0] <- 0

# Simulate missing student data:
math_englearner <- math_englearner %>%
  uncount(total_count) %>%
  group_by(division_name, label, grade, pass_rate, test_year, pass_count) %>%
  mutate(id = 1:n(),
         pass = case_when(
           id <= pass_count ~ 1,
           TRUE ~ 0))  

# Create nested data frame with only columns relevant to the model (one df per year):
math_englearner_byyear <- math_englearner %>% 
  select(label, division_name, test_year, pass) %>%
  group_by(test_year) %>% 
  nest()

# Estimate models: takes time (takes ~30-60 mins)
math_englearner_byyear <- math_englearner_byyear %>% 
  mutate(model = map(data, yearly_model))

# Intermediary step if need to return to modeled data at later point (ie RStudio crash)
# Save RDS file
saveRDS(math_englearner_byyear, "tempdata/math_englearner_byyear.RDS")
# Read in previously saved RDS file

# Get coefficients:
math_englearner_coefs <- math_englearner_byyear %>%
  mutate(
    coefficients = map(model, ~coef(.x)$division_name %>%
                         data.frame() %>%
                         rownames_to_column() %>%
                         tibble() %>%
                         rename(
                           division_name = 1,
                           intercept = 2,
                           label = 3
                         ) %>%
                         mutate(
                           english_learner = exp(intercept)/(1 + exp(intercept)),
                           non_english_learner = exp(intercept + label)/(1 + exp(intercept + label))
                         )
    )
  ) %>%
  select(-data, -model) %>%
  unnest(coefficients)

# Format:
math_englearner_coefs <- math_englearner_coefs %>%
  select(test_year, division_name, english_learner, non_english_learner) %>%
  gather(
    label, rate, -test_year, -division_name
  ) %>%
  mutate(group = "English Learner Status")

# Save:
write_csv(math_englearner_coefs, "model_data/math_englearner.csv")

# Remove large df from environment
rm(math_englearner_byyear)

# Export ----

# Combine:
math_race <- read_csv("model_data/math_race.csv")
math_disadvan <- read_csv("model_data/math_disadvan.csv")
math_englearner <- read_csv("model_data/math_englearner.csv")

math_gap_all <- bind_rows(list(math_race, math_disadvan, math_englearner))

# Format:
math_gap <- math_gap_all %>%
  group_by(group, test_year, division_name) %>%
  mutate(dif = abs(rate - lag(rate)),
         dif = case_when(
           is.na(dif) ~ 0,
           TRUE ~ dif
         ),
         dif = sum(dif),
         max_rate = max(rate)) %>%
  arrange(group, test_year, division_name, label) %>%
  group_by(group, test_year) %>%
  arrange(desc(dif) )%>%
  mutate(rank = ceiling((1:n())/2),
         label = str_to_title(str_replace_all(label, "_", " "))) %>%
  left_join(division_list)

  
# Save:
write_csv(math_gap, "math_gap_data.csv")

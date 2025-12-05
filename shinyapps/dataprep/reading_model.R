## Purpose of script: to simulate the missing READING data and then model for all VA students.

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

# Filter for reading
reading <- complete_sol %>% filter(subject == "English:Reading")

# Get divisions
division_list <- reading %>%
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
reading_race <- reading %>%
  filter(group == "Race")

# Explore missing data:
# In theory, the random coefficients helps draw those measures towards the center 
# so that helps. But at the same time, a bit precarious. 
reading_race %>%
  mutate(missing = case_when(
    is.na(pass_count) ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(test_year, label) %>%
  summarize(missing = mean(missing)) %>%
  spread(test_year, missing)

reading_race %>%
  filter(is.na(pass_count)) %>%
  pull(division_name) %>%
  unique()

# If the numerator equals zero/is not measured then set the denominator to 0 as well
# to not be measured (make the data point missing as opposed to having no one pass the exam)
reading_race[is.na(reading_race)] <- 0 
reading_race$total_count[reading_race$pass_count == 0] <- 0  

# Simulate missing student data:
reading_race <- reading_race %>%
  uncount(total_count) %>%
  group_by(division_name, label, grade, pass_rate, test_year, pass_count) %>%
  mutate(id = 1:n(),
         pass = case_when(
           id <= pass_count ~ 1,
           TRUE ~ 0))

# Create nested data frame with only columns relevant to the model (one df per year):
reading_race_byyear <- reading_race %>% 
  select(label, division_name, test_year, pass) %>%
  group_by(test_year) %>% 
  nest()

# Estimate models: takes a lot of RAM 
reading_race_byyear <- reading_race_byyear %>% 
  mutate(model = map(data, yearly_model))

# Intermediary step if need to return to modeled data at later point (ie RStudio crash)
# Save RDS file
saveRDS(reading_race_byyear, "tempdata/reading_race_byyear.RDS" )
# Read in previously saved RDS file
# reading_race_byyear <- readRDS("tempdata/reading_race_byyear.RDS")

# Get model coefficients:
reading_race_coefs <- reading_race_byyear %>%
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
reading_race_coefs <- reading_race_coefs %>%
  select(test_year, division_name, black, white) %>%
  gather(
    label, rate, -test_year, -division_name
  ) %>%
  mutate(group = "Race")
  
# Save:
write_csv(reading_race_coefs, "model_data/reading_race.csv")

# Remove large df from environment
rm(reading_race_byyear)
 
## Economic Status ----

# Split into subgroups:Economic Status
reading_disadvan <- reading %>%
  filter(group == "Economic Status")

# Explore missing data:
reading_disadvan %>%
  mutate(missing = case_when(
    is.na(pass_count) ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(test_year, label) %>%
  summarize(missing = mean(missing)) %>%
  spread(test_year, missing)

reading_disadvan %>%
  filter(is.na(pass_count)) %>%
  pull(division_name) %>%
  unique()

# If the numerator equals zero/is not measured then set the denominator to 0 as well
# to not be measured (make the data point missing as opposed to having no one pass the exam)
reading_disadvan[is.na(reading_disadvan)] <- 0 
reading_disadvan$total_count[reading_disadvan$pass_count == 0] <- 0  

# Simulate missing student data:
reading_disadvan <- reading_disadvan %>%
  uncount(total_count) %>%
  group_by(division_name, label, grade, pass_rate, test_year, pass_count) %>%
  mutate(id = 1:n(),
         pass = case_when(
           id <= pass_count ~ 1,
           TRUE ~ 0)) 

# Create nested data frame with only columns relevant to the model (one df per year):
reading_disadvan_byyear <- reading_disadvan %>% 
  select(label, division_name, test_year, pass) %>%
  group_by(test_year) %>% 
  nest()
 
# Estimate models: takes time
reading_disadvan_byyear <- reading_disadvan_byyear %>% 
  mutate(model = map(data, yearly_model))

# Intermediary step if need to return to modeled data at later point (ie RStudio crash)
# Save RDS file
saveRDS(reading_disadvan_byyear, "tempdata/reading_disadvan_byyear.RDS")
# Read in previously saved RDS file
# reading_disadvan_byyear <- readRDS("tempdata/reading_disadvan_byyear.RDS")

# Get coefficients:
reading_disadvan_coefs <- reading_disadvan_byyear%>%
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
reading_disadvan_coefs <- reading_disadvan_coefs %>%
  select(test_year, division_name, disadvantaged, non_disadvantaged) %>%
  gather(
    label, rate, -test_year, -division_name
  ) %>%
  mutate(group = "Economic Status")

# Save:
write_csv(reading_disadvan_coefs, "model_data/reading_disadvan.csv")

# Remove large df from environment
rm(reading_disadvan_byyear)

## English Learner Status ----

# Split into subgroups: English Learner Status 
reading_englearner <- reading %>%
  filter(group == "English Learner Status")

# Explore missing values:
reading_englearner %>%
  mutate(missing = case_when(
    is.na(pass_count) ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(test_year, label) %>%
  summarize(missing = mean(missing)) %>%
  spread(test_year, missing)

reading_englearner %>%
  filter(is.na(pass_count)) %>%
  pull(division_name) %>%
  unique()

# If the numerator equals zero/is not measured then set the denominator to 0 as well
# to not be measured (make the data point missing as opposed to having no one pass the exam)
reading_englearner[is.na(reading_englearner)] <- 0
reading_englearner$total_count[reading_englearner$pass_count == 0] <- 0

# Simulate missing student data:
reading_englearner <- reading_englearner %>%
  uncount(total_count) %>%
  group_by(division_name, label, grade, pass_rate, test_year, pass_count) %>%
  mutate(id = 1:n(),
         pass = case_when(
           id <= pass_count ~ 1,
           TRUE ~ 0))  

# Create nested data frame with only columns relevant to the model (one df per year):
reading_englearner_byyear <- reading_englearner %>% 
  select(label, division_name, test_year, pass) %>%
  group_by(test_year) %>% 
  nest()

# Estimate models: takes time
reading_englearner_byyear <- reading_englearner_byyear %>% 
  mutate(model = map(data, yearly_model))

# Intermediary step if need to return to modeled data at later point (ie RStudio crash)
# Save RDS file
saveRDS(reading_englearner_byyear, "tempdata/reading_englearner_byyear.RDS")
# Read in previously saved RDS file

# Get coefficients:
reading_englearner_coefs <- reading_englearner_byyear %>%
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
reading_englearner_coefs <- reading_englearner_coefs %>%
  select(test_year, division_name, english_learner, non_english_learner) %>%
  gather(
    label, rate, -test_year, -division_name
  ) %>%
  mutate(group = "English Learner Status")

# Save:
write_csv(reading_englearner_coefs, "model_data/reading_englearner.csv")

# Remove large df from environment
rm(reading_englearner_byyear)

# Export ----

# Combine:
reading_race <- read_csv("model_data/reading_race.csv")
reading_disadvan <- read_csv("model_data/reading_disadvan.csv")
reading_englearner <- read_csv("model_data/reading_englearner.csv")

reading_gap_all <- bind_rows(list(reading_race, reading_disadvan, reading_englearner))

# Format:
reading_gap <- reading_gap_all %>%
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
write_csv(reading_gap, "reading_gap_data.csv")

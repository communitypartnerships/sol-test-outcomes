# Get and Clean Data from VDOE for site
# Includes:
# - Division Pass Rates for 3rd-8th Grade Reading
# - Division Pass Rates for 3rd-8th Grade Math

# Libraries ----
library(here)
library(tidyverse)
library(janitor)
library(boxr)

# Set WD ----
setwd(here("shinyapps/dataprep"))

# Box authentication ----
readRenviron("~/.Renviron")
box_auth(client_id = Sys.getenv('BOX_CLIENT_ID'), client_secret = Sys.getenv('BOX_CLIENT_SECRET'))

# Create temp data folder
if (!dir.exists("tempdata")) {
  # If it does not exist, create the directory
  dir.create("tempdata", recursive = TRUE)
  message(paste("Directory created:", "tempdata"))
} else {
  message(paste("Directory already exists:", "tempdata"))
}

# Get Data from Box ----
dir_id <- "342340785082" # SOL Test Outcomes Box folder ID
box_setwd(dir_id)

box_fetch(
  dir_id = box_getwd(),
  local_dir = "tempdata",
  recursive = TRUE,
  overwrite = TRUE,
  delete = FALSE
)

# Clean Data ----
all_raw <- read_csv("tempdata/all.csv") %>% 
  clean_names()

all <- all_raw %>% 
  mutate(group = "Overall",
         label = "All Students",
         test_year = as.numeric(str_sub(school_year, 6,9)),
         grade = as.numeric(str_sub(test_level, 7,7))) %>% 
  mutate(across(.cols = contains("count"), .fns = ~as.numeric(str_replace_all(.x, ",", "")))) %>% 
  mutate(across(contains("rate"), as.numeric)) %>% 
  select(school_year, level, division_number, division_name, subject, test_level, test, group, label,
         pass_count, total_count, pass_rate, test_year, grade)

disadvantaged_raw <- read_csv("tempdata/disadvantaged.csv") %>% 
  clean_names()

disadvantaged <- disadvantaged_raw %>% 
  mutate(group = "Economic Status",
         label = case_when(disadvantaged == "Y" ~ "Disadvantaged",
                           disadvantaged == "N" ~ "Non Disadvantaged"),
         test_year = as.numeric(str_sub(school_year, 6,9)),
         grade = as.numeric(str_sub(test_level, 7,7))) %>% 
  mutate(across(.cols = contains("count"), .fns = ~as.numeric(str_replace_all(.x, ",", "")))) %>% 
  mutate(across(contains("rate"), as.numeric)) %>% 
  select(school_year, level, division_number, division_name, subject, test_level, test, group, label,
         pass_count, total_count, pass_rate, test_year, grade)

englishlearner_raw <- read_csv("tempdata/englishlearner.csv") %>% 
  clean_names()

englishlearner <- englishlearner_raw %>% 
  mutate(group = "English Learner Status",
         label = case_when(english_learners == "Y" ~ "English Learner",
                           english_learners == "N" ~ "Non English Learner"),
         test_year = as.numeric(str_sub(school_year, 6,9)),
         grade = as.numeric(str_sub(test_level, 7,7))) %>% 
  mutate(across(.cols = contains("count"), .fns = ~as.numeric(str_replace_all(.x, ",", "")))) %>% 
  mutate(across(contains("rate"), as.numeric)) %>% 
  select(school_year, level, division_number, division_name, subject, test_level, test, group, label,
         pass_count, total_count, pass_rate, test_year, grade)

race_raw <- read_csv("tempdata/race.csv") %>% 
  clean_names()

race <- race_raw %>% 
  mutate(group = "Race",
         label = case_when(race == "Black, not of Hispanic origin" ~ "Black",
                           race == "White, not of Hispanic origin" ~ "White"),
         test_year = as.numeric(str_sub(school_year, 6,9)),
         grade = as.numeric(str_sub(test_level, 7,7))) %>% 
  mutate(across(.cols = contains("count"), .fns = ~as.numeric(str_replace_all(.x, ",", "")))) %>% 
  mutate(across(contains("rate"), as.numeric)) %>% 
  select(school_year, level, division_number, division_name, subject, test_level, test, group, label,
         pass_count, total_count, pass_rate, test_year, grade)

# Combine ----
complete_sol <- rbind(all, disadvantaged, englishlearner, race)

# Update Division Names: VA Overall and update Alleghany County/Highlands Division
complete_sol <- complete_sol %>% 
  mutate(division_name = case_when(division_name == "Alleghany County" ~ "Alleghany Highlands",
                                   .default = division_name),
         division_name = case_when(level=="State" ~ "VA Overall",
                                   .default = division_name))

# Save ----
write_csv(complete_sol, "complete_sol_data.csv")

# Data prep for Division Pass Rates Shiny App
# Includes:
# - Division Pass Rates for 3rd-8th Grade Reading
# - Division Pass Rates for 3rd-8th Grade Math

# Libraries ----
library(here)
library(tidyverse)
library(janitor)

# Set WD ----
setwd(here("shinyapps/pass-rates"))

# Get Data ----
complete_sol <- read_csv("../dataprep/complete_sol_data.csv")

# Filter for Overall
overall <- complete_sol %>% 
  filter(group=="Overall")

# Add data rows for 2019-2020 No test year
overall_covid <- overall %>% 
  select(level, division_number, division_name, subject, test_level, test, group, label, grade) %>% 
  unique() %>% 
  mutate(school_year = "2019-2020",
         test_year = 2020,
         pass_count = NA,
         total_count = NA,
         pass_rate = NA) %>% 
  select(school_year, level, division_number, division_name, subject, test_level, test, group, label,
         pass_count, total_count, pass_rate, test_year, grade)

# Add 2020 NA values
overall <- rbind(overall, overall_covid)

# Save app data
write_csv(overall, "pass_rates_data.csv")

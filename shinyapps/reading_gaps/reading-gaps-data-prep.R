# Data prep for Reading Gaps Shiny app

# Libraries ----
library(here)
library(tidyverse)
library(janitor)

# Set WD ----
setwd(here("shinyapps/reading-gaps"))

# Get Data ----
reading_gap <- read_csv("../dataprep/reading_gap.csv")

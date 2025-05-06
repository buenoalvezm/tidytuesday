## Title:     TidyTuesday - week 22 - 2024
## Date:      2024-05-28
## Author:    María Bueno Álvez
## Data:      Lisa's Vegetable Garden Data


## LOAD PACKAGES & SETUP 
library(tidyverse)

# Read in data
tuesdata <- tidytuesdayR::tt_load(2025, week = 14)

## DATA EXPLORATION
care_state <- tuesdata$care_state

## VISUALIZATION
care_state |> 
  filter(measure_name == "Average (median) time patients spent in the emergency department before leaving from the visit A lower number of minutes is better") |> 
  filter(!is.na(score)) |> 
  ggplot(aes(state, score)) +
  geom_col()
  
# Connection between state populations and wait times?
# Conditions that have the longest and shortest wait times?
  

## SESSION INFO  
sessioninfo::session_info(include_base = TRUE) 
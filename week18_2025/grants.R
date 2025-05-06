## Title:     TidyTuesday - week 18 - 2025
## Date:      2025-05-06
## Author:    María Bueno Álvez
## Data:      National Science Foundation Grant Terminations under the Trump Administration


## LOAD PACKAGES & SETUP 
library(tidyverse)
library(patchwork)

# Read in data
tuesdata <- tidytuesdayR::tt_load(2025, week = 18)

## DATA EXPLORATION
grants <- tuesdata$nsf_terminations

grants_dat <- 
  grants

## VISUALIZATION
# How many grants, and how much money, were terminated by state or congressional district? What institutions? How can you present these on a map?
# Grants from what directorates, divisions, or programs made up most of the projects terminated?
# What topics or terms are most common in project titles or abstracts?

# Worlclous
# Heatmap - country

  

# Can you detect any correlations between fatal car crashes and particular days of the year?
# What are the most dangerous days of the year for fatal car crashes in the United States?
# What other factors might help analyze the data in more detail? You can use the cleaning script to download the full dataset.

## SESSION INFO  
sessioninfo::session_info(include_base = TRUE) 

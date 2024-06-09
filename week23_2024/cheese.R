## Title:     TidyTuesday - week 23 - 2024
## Date:      2024-06-09
## Author:    María Bueno Álvez
## Data:      Cheeses (cheese.com)


## LOAD PACKAGES & SETUP 
#install.packages(c("maps", "mapdata"))
library(tidyverse)
library(maps)
library(mapdata)

# Read in data
tuesdata <- tidytuesdayR::tt_load(2024, week = 23)
cheeses <- tuesdata$cheeses

## DATA EXPLORATION
cheese_data <- 
  cheeses |> 
  separate_rows(country, sep = ", ") |> 
  separate_rows(milk, sep = ", ") |>
  distinct(cheese, milk, country) |> 
  group_by(country) |> 
  mutate(total_n = n_distinct(cheese)) |> 
  ungroup() |> 
  group_by(country, milk) |> 
  mutate(n = n()) |> 
  ungroup() |> 
  distinct(country, milk, total_n, n)

## VISUALIZATION

# Non-mapping countries
cheese_data |> 
  distinct(country) |> 
  filter(!country %in% country_coords$region)

# Retrieve world map data
world_map <- map_data("world")

country_coords <- 
  world_map %>%
  group_by(region) %>%
  summarize(long = mean(long), lat = mean(lat))

# Current data
current_countries <- 
  country_coords |> 
  filter(region %in% countries)

# Final plot
world_map |> 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "lightgrey", 
               color = "white") +
  geom_point(data = current_countries, 
             aes(x = long, y = lat), 
             color = "red",
             size = 2, 
             inherit.aes = F) +
  theme_minimal() 


## SESSION INFO  
sessioninfo::session_info(include_base = TRUE) 

# map - 62 countries
# size - number of cheeses
# pie - proportion milk type

 


# Beautiful layout
# Titles & text
# Socials

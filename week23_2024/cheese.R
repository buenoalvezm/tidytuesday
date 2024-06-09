## Title:     TidyTuesday - week 23 - 2024
## Date:      2024-06-09
## Author:    María Bueno Álvez
## Data:      Cheeses (cheese.com)


## LOAD PACKAGES & SETUP 
#install.packages(c("maps", "mapdata"))
library(tidyverse)
library(maps)
library(mapdata)
library(scatterpie)
library(ggsci)
library(patchwork)

# Read in data
tuesdata <- tidytuesdayR::tt_load(2024, week = 23)
cheeses <- tuesdata$cheeses



## DATA EXPLORATION

# Non-mapping countries
cheese_data |> 
  distinct(country) |> 
  filter(!country %in% country_coords$region)

# Rename or exclude countries & count number of distinct chesses and number of cheeses from different milk types
cheese_data <- 
  cheeses |> 
  separate_rows(country, sep = ", ") |> 
  separate_rows(milk, sep = ", ") |>
  distinct(cheese, milk, country) |> 
  filter(!country %in% c("Middle East", "Mexico and Caribbean"),
         !is.na(country),
         !is.na(milk)) |> 
  mutate(country = case_when(country %in% c("England", "Great Britain", "United Kingdom", "Wales", "Scotland") ~ "UK", 
                             country == "United States" ~ "USA",
                             country == "Holland" ~ "Netherlands",
                             country == "Macedonia" ~ "North Macedonia",
                             T ~ country)) |> 
  group_by(country) |> 
  mutate(total_n = n_distinct(cheese)) |> 
  ungroup() |> 
  group_by(country, milk) |> 
  mutate(n = n()) |> 
  ungroup() |> 
  distinct(country, milk, total_n, n)



## VISUALIZATION

# Retrieve world map data
world_map <- map_data("world")

country_coords <- 
  world_map %>%
  group_by(region) %>%
  summarize(long = mean(long), lat = mean(lat)) |> 
  bind_rows(tibble(region = "Tibet", # Add Tibet as region
                   long = 85,
                   lat = 35))

# Current data
current_countries <- 
  country_coords |> 
  filter(region %in% unique(cheese_data$country))

# Focus on Europe
europe_cheese <- 
  cheese_data |> 
  left_join(country_coords, by = c("country" = "region")) |> 
  relocate(long, lat, .after = country) |> 
  filter(long < 40,
         long > -30,
         lat < 75,
         lat > 30)  |> 
  pivot_wider(names_from = milk, values_from = n, values_fill = 0) 
  

# Map plot
map <- 
  world_map |> 
  filter(long < 40,
         long > -30,
         lat < 75,
         lat > 30) |> 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "lightgrey", 
               color = "white") +
  geom_scatterpie(aes(x=long, y=lat, group=country), 
                  data = europe_cheese, 
                  cols = colnames(europe_cheese)[-c(1:4)],
                  alpha = 0.7) +
  scale_fill_aaas() +
  theme_void() 

# Add title and subtitle
# Add pie de foto 
# Fix coordinates

# Barplot most common milk
europe_cheese


# Barplot top countries
europe_cheese


# Final plot
milk_plot + map + country_plot


## SESSION INFO  
sessioninfo::session_info(include_base = TRUE) 

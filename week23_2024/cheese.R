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
library(showtext)
library(ggimage)

# Read in data
tuesdata <- tidytuesdayR::tt_load(2024, week = 23)
cheeses <- tuesdata$cheeses

# Add font
font_add_google("Roboto", "roboto")


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
  

social <- nrBrand::social_caption(
  bg_colour = bg_col,
  icon_colour = highlight_col,
  font_colour = text_col,
  font_family = body_font
)



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
  theme_void() +
  labs(
    title = "Cheese types in Europe",
    subtitle = "Based on data from cheese.com"
  ) +
  theme(
    plot.title = element_text(
      family  = "roboto",
      face = "bold", 
      hjust = 0.5,
      size = 24),
    plot.subtitle = element_text(
      family  = "roboto",
      hjust = 0.5,
      size = 14)
  )
  

# Social media details
social_media <- data.frame(
  platform = c("twitter", "linkedin", "github"),
  handle = c("@_buenoalvez", "María Bueno Álvez", "buenoalvezm"),
  x = c(0, 0.5, 1), 
  y = c(0, 0, 0),
  #x = c(1, 1, 1),  # Dummy x-axis values for positioning
  #y = c(3, 2, 1),  # Dummy y-axis values for positioning
  image = c("week23_2024/assets/twitter.png", "week23_2024/assets/linkedin.png", "week23_2024/assets/github.png")  
) 



# Plot with social media icons 
#FIX ASPECT RATIO
# LEGEND
p_social <- 
  ggplot(social_media, aes(x, y)) +
  geom_image(aes(image = image), size = 0.05, asp = 1/1) +
  geom_text(aes(label = handle, x + 0.05), hjust = 0, vjust = 0.5, size = 5, family = "roboto") +
 # expand_limits(y=c(-0.01, 0.01)) +
  expand_limits(x=c(-0.3, 1.3)) +
  theme_void()


map / p_social +
  plot_layout(heights = c(1,0.5))

# Barplot most common milk
europe_cheese


# Barplot top countries
europe_cheese


# Final plot
milk_plot + map + country_plot


## SESSION INFO  
sessioninfo::session_info(include_base = TRUE) 

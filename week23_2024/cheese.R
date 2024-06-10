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
  filter(long < 30,
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
social_media <- data.frame(
  platform = c("twitter", "linkedin", "github"),
  handle = c("@_buenoalvez", "María Bueno Álvez", "buenoalvezm"),
  long = c(-20, 0, 20), 
  lat = c(25, 25, 25),
  image = c("week23_2024/assets/twitter.png", "week23_2024/assets/linkedin.png", "week23_2024/assets/github.png")  
) 


world_map |> 
  filter(long < 30,
         long > -30,
         lat < 75,
         lat > 30) |> 
  ggplot(aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), 
               fill = "lightgrey", 
               color = "white") +
  geom_scatterpie(aes(x=long, y=lat, group=country), 
                  data = europe_cheese, 
                  pie_scale = 1,
                  cols = colnames(europe_cheese)[-c(1:4)],
                  
                  alpha = 0.7) +
  geom_image(aes(image = image), size = 0.03, by = "height", asp = 1, data = social_media) +
  geom_text(aes(label = handle, x = long + 2), hjust = 0, vjust = 0.5, size = 3, family = "roboto", data = social_media) + 
  scale_fill_aaas() +
  theme_void() +
  labs(
    title = "Cheese types in Europe",
    subtitle = "Based on data from cheese.com",
    fill = "Cheese type"
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
      size = 14),
    legend.position = c(0.10, 0.20),
    legend.title = element_text(family = "roboto", size = 12, face = "bold"),
    legend.text = element_text(family = "roboto", size = 10),
    legend.justification = c(0, 0),
    aspect.ratio = 1
  ) +
  coord_cartesian(
    xlim = c(-30, 40),
    ylim = c(30, 75),
    expand = FALSE
  ) +
  coord_equal() 

#ggsave("week23_2024/cheese_map.png", h = 14, w = 14)

## SESSION INFO  
sessioninfo::session_info(include_base = TRUE) 

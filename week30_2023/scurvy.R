library(tidyverse)

data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-25/scurvy.csv')

title <- "Scurvy trial by James Lind"
subtitle <- "Data collected in 1757 during the study of 12 seamen with scurvy, a manifestation of vitamin C deficiency.\n\n"

plot_data <- 
  data |> 
  select(study_id, 
         Treatment = treatment, 
         Fit = fit_for_duty_d6,
         `Gum rot` = gum_rot_d6, 
         `Skin score` = skin_sores_d6, 
         `Weakness of the knees` = weakness_of_the_knees_d6, 
         `Lassitude` = lassitude_d6) |>
  mutate(Fit = case_when(Fit == "0_no" ~ "No",
                         Fit == "1_yes" ~ "Yes"),
         Treatment = case_when(Treatment == "cider" ~ "Cider",
                               Treatment == "citrus" ~ "Citrus",
                               Treatment == "dilute_sulfuric_acid" ~ "Sulfuric acid",
                               Treatment == "purgative_mixture" ~ "Purgative mixture",
                               Treatment == "sea_water" ~ "Sea water",
                               Treatment == "vinegar" ~ "Vinegar")) |> 
  pivot_longer(c(4:7), names_to = "symptom", values_to = "value") |> 
  group_by(study_id) |> 
  mutate(score = case_when(value == "0_none" ~ 0,
                           value == "1_mild" ~ 1,
                           value == "2_moderate" ~ 2,
                           value == "3_severe" ~ 3),
           total_score = sum(score))

plot_data |> 
  mutate(study_id = factor(study_id)) |> 
  ggplot(aes(fct_reorder(study_id, total_score), symptom, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_manual("Scores", 
                    values = c("0_none" = "#FCE7EB",
                               "1_mild" = "#D49DC5",
                               "2_moderate" = "#8267AB",
                               "3_severe" = "#5D449A")
  ) +
  ggnewscale::new_scale_fill() +
  geom_tile(aes(study_id, " Fit", fill = Fit), color = "white") + 
  scale_fill_manual(values = c("Yes" = "#ADC74F",
                               "No" = "grey90")) +
  ggnewscale::new_scale_fill() +
  geom_tile(aes(study_id, " Treatment", fill = Treatment), color = "white") +
  scale_fill_manual(values = c("#EA4649","#FCC174", "#E7662B", 
                              "#A6CEE3", "#2271B5", "#B89B74")) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    legend.box = "horizontal",
    legend.title = element_text(face = "bold")    
    ) +
  coord_fixed() +
  scale_x_discrete(position = "top") +
  labs(
    x = "Participants", 
    y = "",
    title = title,
    subtitle = subtitle,
    caption = "tidytuesday 2023/07/25 - María Bueno Álvez"
  )

ggsave("week30_2023/scurvy.png", h = 5, w = 10)

  

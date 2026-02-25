library(tidyverse)
library(sysfonts)
library(showtext)

tuesdata <- tidytuesdayR::tt_load(2026, week = 5)

edible_plants <- tuesdata$edible_plants

# corregir agua
edible_plants <- edible_plants |>
  mutate(
    water = str_to_title(water),
    water = factor(water, levels = c("Very High", "High", "Medium", "Low", "Very Low"))
    )


# edible_plants |>
#   mutate(
#     sunlight = str_to_title(str_replace(sunlight, "/ ", "/"))
#     ) |>
#   ggplot(aes(y= sunlight)) +
#   geom_bar()
  
# agregar cols temp
edible_plants <- edible_plants |>
  mutate(
    min_temp_grow = as.numeric(str_extract(temperature_growing, "^[0-9]{1,2}")),
    max_temp_grow = as.numeric(str_extract(temperature_growing, "[0-9]{1,2}$")),
    mean_temp_grow = (min_temp_grow + max_temp_grow) / 2
  )


edible_plants |>
  filter(!is.na(mean_temp_grow)) |>
  group_by(cultivation) |>
  summarise(
    n = n(),
    mean_temp = mean(mean_temp_grow, na.rm = TRUE),
    min_temp = min(min_temp_grow, na.rm = TRUE),
    max_temp = max(max_temp_grow, na.rm = TRUE)
  ) |>
  mutate(
    cultivation_labeled = paste0(cultivation, "\n(n=", n, ")"),
  ) |>
  ungroup() |>
  ggplot(aes(
    y = fct_reorder(cultivation_labeled, mean_temp), 
    x = mean_temp,
    xmin = min_temp,
    xmax = max_temp)) + 
  geom_pointrange(linewidth = 2, color = "orangered", alpha = 0.3) +
  geom_point(size = 3, color = "orangered") +
  labs(
    x = "Growing temperature (°C)",
    title = "Growing temperature of edible plants",
    subtitle = "Range of growing temperature of 55 edible plants. Mean growing\ntemperature for all cultivar groups ranges between 15 °C and 25 °C.",
    caption = "Data: Edible Plants Database"
  ) +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(size = 17, face = "bold", margin = margin(b = 15)),
    plot.subtitle = element_text(margin = margin(b = 10)),
    axis.title.y = element_blank(),
    axis.text.y = element_text(hjust = 0, color = "black"),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0, margin = margin(t = 15)),
    plot.margin = margin(0.7,0.7,0.7,0.7, "cm")
    ) 

ggsave(
  "temp_edible_plants.png", 
  width = 14, 
  height = 13, 
  units = "cm",
  dpi = 300
  )

# ph
# nutrients
# water
# sunlight

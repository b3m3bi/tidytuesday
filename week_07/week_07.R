library(tidyverse)
library(ggstream)
library(paletteer)

tuesdata <- tidytuesdayR::tt_load(2026, week = 7)
dataset <- tuesdata$dataset

color_palette <- rev(c("#004b23", "#006400", "#008000", "#70e000", "#ccff33"))

df <- dataset |>
  filter(str_detect(measure, "area sown") & year_ended_june >= 1970 & year_ended_june <= 2002) |>
  group_by(measure) |>
  mutate(
    total_area_sown = sum(value),
    last_area_sown = value[which.max(year_ended_june)]
  )|> 
  ungroup() |> 
  mutate(
    crop = str_remove(measure, " \\(area sown\\)"),
    crop = if_else(crop == "Field peas", "Peas", crop)
  )

df |> 
  filter(year_ended_june == max(year_ended_june)) |>
  mutate(prop = value / sum(value) * 100)
  
df_labels <- df |> 
  group_by(crop) |> 
  filter(year_ended_june == max(year_ended_june)) |> 
  ungroup()

51.1+ 27.6

ggplot(
    data = df, 
    aes(x = year_ended_june, y = value, fill = fct_reorder(crop, last_area_sown))
  ) +
  geom_area(color = "white", linewidth = 0.5) +
  geom_text(
    data = df_labels,
    aes(label = crop),
    position = position_stack(vjust = 0.5),
    hjust = -0.1,
    size = 3.5
  ) + 
  guides(fill = "none") +
  labs(
    x = "Year",
    y = "Area (ha)",
    fill = "Crop",
    title = "Area sown in New Zealand from 1970 to 2002",
    subtitle = "Total area sown in New Zealand has decreased. Main crops in 2002 were\nbarley and wheat, representing more than 78% of the sown area.",
    caption = "Data: Stats NZ. Agriculutral Production. TidyTuesday 2026 week 7."
  ) +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_x_continuous(breaks = seq(1970, 2000, by = 5), expand = expansion(add = c(0, 3))) +
  # scale_fill_manual(
  #   values = color_palette
  # ) + 
  scale_fill_paletteer_d("fishualize::Cirrhilabrus_solorensis") +
  scale_fill_paletteer_d("MoMAColors::Budnitz") +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(margin = margin(b = 10), face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 10)),
    plot.caption = element_text(hjust = 0, margin = margin(t = 15)),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 8, color = "black", margin = margin(r = 8)),
    axis.text.x = element_text(size = 8, color = "black")
  )

ggsave("new_zealand_agriculture.png", width = 6, height = 6)



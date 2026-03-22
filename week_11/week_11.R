library(tidyverse)
library(lubridate)
library(ggtext)

tuesdata <- tidytuesdayR::tt_load(2026, week = 11)

monthly_losses_data <- tuesdata$monthly_losses_data
monthly_mortality_data <- tuesdata$monthly_mortality_data



total_losses <- monthly_losses_data |>
  filter(species == "salmon" & geo_group == "county") |>
  select(region, losses) |>
  filter(losses != 0)|>
  group_by(region) |>
  summarise(
    total_loss = sum(losses)
  )

plot_data <- monthly_losses_data |>
  filter(species == "salmon" & geo_group == "county") |>
  pivot_longer(
    cols = c(dead, discarded, escaped, other),
    names_to = "loss_type",
    values_to = "loss_val"
  ) |> 
  mutate(
    loss_type = factor(loss_type, levels = c("dead", "other", "discarded", "escaped"))
  ) |>
  group_by(region, species, date, loss_type) |>
  summarise(
    loss = sum(loss_val)
  ) |>
  filter(loss != 0) |>
  left_join(total_losses) |> 
  mutate(
    region = fct_reorder(region, total_loss)
  )


plot_data |>
  group_by(loss_type) |>
  summarise(
    total_losses = sum(loss)
  )

title_text <- "Salmon losses in Norway"
subtitle_text <- "The main cause of salmon losses in Norwegian fish farms are deaths. Farms in Vestland report the highest number of losses. Some peaks in losses are driven by external mortality factors, such as algal blooms."
caption_text <- "Data: Norwegian Veterinary Institute. TidyTuesday week 11."

annot <- tibble(
  region = "Nordland",
  date = as.Date("2023-01-01"),
  loss = 3000000,
  text = "2025 algal bloom"
)

plot_data |>  
  ggplot(
    aes(x = date, y = loss)
  ) +
  geom_area(aes(fill = loss_type)) +
  geom_text(
    data = annot,
    aes(label = text),
    size = 3
  ) +
  geom_curve(
    data = tibble(
      region = "Nordland",
      date = as.Date("2023-10-01"),
      loss = 3000000,
      xend = as.Date("2025-05-01"),
      yend = 3500000
    ),
    aes(x= date, xend = xend, y = loss, yend = yend),
    curvature = -0.2,
    arrow = arrow(length = unit(0.1, "npc"))
  ) +
  # facet_wrap(vars(fct_reorder(region, total_loss))) +
  facet_wrap(vars(region)) + 
  labs(
    x = "Date",
    y = "Losses",
    title = title_text,
    subtitle = subtitle_text,
    fill = "Cause of loss",
    caption = caption_text
  ) +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  theme_minimal(base_family = "Google Sans") +
  theme(
    legend.position = 'top', 
    legend.spacing.x = unit(1.0, 'cm'),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_textbox_simple(hjust = 0, color = "gray30", margin = margin(t = 15)),
    plot.title = element_text(face = "bold", size = 14, margin = margin(b = 15)),
    plot.subtitle = element_textbox_simple(margin = margin(b = 15)),
    plot.margin = unit(c(10, 10, 10, 10), 'pt'),
    panel.grid.minor = element_blank(),
    panel.spacing.x = unit(15, "pt"),
  )
  
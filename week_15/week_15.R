library(tidyverse)
library(lubridate)

sysfonts::font_add("google sans", "GoogleSans-Regular.ttf", bold = "GoogleSans-Bold.ttf", italic = "GoogleSans-Italic.ttf")
showtext::showtext.auto()
showtext::showtext_opts(dpi = 96)

tuesdata <- tidytuesdayR::tt_load(2026, week = 15)

beaufort_scale <- tuesdata$beaufort_scale
birds <- tuesdata$birds
sea_states <- tuesdata$sea_states
ships <- tuesdata$ships

## Preguntas:
# - ¿Los pájaros avistados cambian con las condiciones del viento/horario/condición de mar/olas/temperatura/precipitación/temporada?
# - ¿En que condiciones de viento/horario se avistan más aves?
# - ¿La edad de los pájaros avistados cambia con la fecha/viento/horario?
# - ¿La actividad del pájaro varía con el horario/temporada?
# - ¿En qué horarios hay más avistamientos?


ships |>
  select(date) |>
  mutate(
    month = month(date)
  ) |>
  count(month) |>
  ggplot(aes(x = month, y = n)) +
  geom_col() + 
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(
    y = "Number of observation trips"
  )

ships |>
  select(date, record_id) |>
  left_join(birds |> select(count, record_id)) |>
  mutate(
    month = month(date)
  ) |>
  summarise(
    total_birds_observed = sum(count, na.rm = TRUE),
    .by = month
  ) |>
  ggplot(aes(x = month, y = total_birds_observed)) +
  geom_col() +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_y_continuous(labels = scales::cut_short_scale()) +
  labs(
    y = "Total observed birds"
  )

ships |> 
  select(time) |>
  ggplot(aes(x = time)) +
  geom_histogram()


birds |>
  select(record_id, count) |>
  left_join(ships |>  select(record_id, time, cloud_cover)) |> 
  count(cloud_cover, wt = count) |>
  drop_na() |> 
  ggplot(aes(x = cloud_cover, y = n)) + 
  geom_col()

birds |>
  select(record_id, count) |>
  left_join(ships |>  select(record_id, time, precipitation)) |> 
  count(precipitation, wt = count) |>
  drop_na() |> 
  ggplot(aes(x = precipitation, y = n)) + 
  geom_col()

birds |>
  select(record_id, count) |>
  left_join(ships |>  select(record_id, time, sea_state_class)) |> 
  count(sea_state_class, wt = count) |>
  drop_na() |> 
  ggplot(aes(x = sea_state_class, y = n)) + 
  geom_col()


birds |>
  select(record_id, count) |>
  left_join(ships |>  select(record_id, time, wind_speed_class)) |> 
  count(wind_speed_class, wt = count) |>
  drop_na() |> 
  ggplot(aes(x = wind_speed_class, y = n)) + 
  geom_col()

birds |>
  select(record_id, count) |>
  left_join(ships |>  select(record_id, time, season)) |> 
  count(season, wt = count) |>
  drop_na() |> 
  ggplot(aes(x = season, y = n)) + 
  geom_col()

birds |>
  select(record_id, count) |>
  left_join(ships |>  select(record_id, date)) |> 
  mutate(
    year = year(date)
  ) |>
  count(year, wt = count) |>
  drop_na() |> 
  ggplot(aes(x = year, y = n)) + 
  geom_col()

color_palette <- rev(c("#012a4a", "#01497c", "#2a6f97", "#468faf", "#89c2d9"))

plot_data <- birds |>
  select(record_id, feeding, sitting_on_water, flying_past, accompanying, following_ship) |>
  left_join(ships |> select(record_id, time)) |> 
  drop_na() |>
  pivot_longer(-c(record_id, time),names_to = "action", values_to = "action_state") |>
  mutate(
    hour = hour(time),
    action_state  = as.numeric(action_state),
    action = action |> recode_values(
      "accompanying" ~ "Flying alongside the ship",
      "feeding" ~ "Feeding",
      "flying_past" ~ "Flying past",
      "following_ship" ~ "Following the ship's wake",
      "sitting_on_water" ~ "Sitting on water"
    )
  ) |>
  group_by(hour, action) |>
  summarise(
    tot = sum(action_state)
  ) |>
  ungroup()


plot_median <- plot_data |>
  group_by(action) |>
  summarise(
    median = sum(matrixStats::weightedMedian(hour, w = tot, na.rm = TRUE)) 
)
  
plot_data <- plot_data |>
  left_join(plot_median, by = "action")


plot_data |> 
  mutate(
    action = fct_reorder(action, median)
  ) |>
  ggplot(aes(x = hour, y = tot, fill = as.factor(median))) + 
  geom_col() + 
  geom_vline(aes(xintercept = median), color = "gray30") + 
  facet_wrap(
    vars(fct_reorder(action, median)),
    ncol = 1,
    axes = "all"
    ) +
  # geom_text(
  #   data = tibble(action = "Feeding"),
  #   aes(x = 14, y = 1200, label = "median"),
  #   size = 3.5,
  #   inherit.aes = FALSE
  # ) + 
  scale_x_continuous(breaks = seq(0, 24, 2)) + 
  scale_fill_manual(values = color_palette) +
  labs(
    title = "Eat in the morning and follow ships in the afternoon",
    subtitle = "Number of seabirds observed exhibiting different behaviors during 10-minute ship surveys at various times of day.",
    x = "Time of the day",
    y = "Number of observed birds",
    caption = "<b>Data</b>: Bird Sightings at Sea - <i>Te Papa Tongarewa</i> (The Museum of New Zealand).<br>TidyTuesday week 15."
  ) +
  theme_minimal() +
  theme(
    margins = margin(t = 10, r = 10, b = 10, l = 10),
    plot.background = element_rect(fill = "#fafafa"),
    text = element_text(family = "google sans"),
    legend.position = "none",
    plot.title.position = "plot",
    plot.title = ggtext::element_textbox_simple(face = "bold", size = 16),
    plot.subtitle = ggtext::element_textbox_simple(margin = margin(t = 15, b = 15)),
    plot.caption.position = "plot",
    plot.caption = ggtext::element_textbox_simple(
      hjust = 0, 
      margin = margin(t = 15),
      color = "gray30",
      size = 8
      ),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(hjust = 0, face = "bold"),
    panel.spacing.y = unit(15, "pt"),
    axis.title.y = element_blank(),
    axis.ticks.x = element_line(color = "gray50", size = 0.2)
  )

showtext::showtext_opts(dpi = 300)
ggsave("week_15.png", dpi = 300, width = 4, height = 8)


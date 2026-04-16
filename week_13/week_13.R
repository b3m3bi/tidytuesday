library(tidyverse)
library(lubridate)


ocean_temperature <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-31/ocean_temperature.csv')
ocean_temperature_deployments <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-31/ocean_temperature_deployments.csv')

oc_temp <- ocean_temperature |> 
  select(-c(sd_temperature_degree_c, n_obs)) |>
  mutate(
    year = year(date),
    month = month(date)
  ) |>
  group_by(year, month, sensor_depth_at_low_tide_m) |>
  summarise(
    mean_temp = mean(mean_temperature_degree_c)
  )|>
  ungroup() |>
  drop_na()


last_Dec <- oc_temp |>
  filter(month == 12) |>
  mutate(year = year + 1,
         month = 0)

next_Jan <- oc_temp |>
  filter(month == 1) |>
  mutate(year = year + 1,
         month = 13)


design <- "
 AB
 CD
 EF
 G#
"

title <- "Ocean Temperature by Depth"
subtitle <- "Annual patterns in mean ocean temperature in Nova Scotia vary with depth. In shallow waters, temperatures peak in September, whereas in deeper waters, the peak occurs in October. Temperature variability is lower at greater depths."

bind_rows(last_Dec, oc_temp) |>
  arrange(year, month) |>
  group_by(sensor_depth_at_low_tide_m) |>
  mutate(
    diff_month = month - lag(month),
    seq_group = cumsum(is.na(diff_month) | diff_month > 1)
  ) |>
  ungroup() |>
  mutate(
    depth = factor(
      paste0(sensor_depth_at_low_tide_m, " m"),
      levels = c("2 m", "5 m", "10 m", "15 m", "20 m", "30 m", "40 m")
    )
  ) |>
  ggplot(
    aes(x = month, y = mean_temp, color = year, group = interaction(year,seq_group))
  ) + 
  geom_line() +
  ggh4x::facet_manual(vars(depth), design = design, respect = TRUE, strip.position = "top") +
  # facet_wrap(vars(depth)) + 
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(
    title = title,
    subtitle = subtitle,
    caption = "Data: Nova Scotia Open Data Portal. TidyTuesday week 13"
  ) +
  paletteer::scale_color_paletteer_c("grDevices::RdYlBu")+
  coord_polar(clip = "off") +
  theme_minimal(base_family = "Roboto") +
  theme(
    plot.margin = margin(l = 20, r = 20, t = 20, b = 20),
    plot.background = element_rect(fill = "#fafafa"),
    plot.title = element_text(face = "bold", margin = margin(b = 15), size = 16),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.subtitle = ggtext::element_textbox_simple(margin = margin(b = 20, t = 10)),
    plot.caption =  ggtext::element_textbox_simple(hjust = 0, margin = margin(t = 20), color = "#888"),
    axis.title = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 8),
    strip.text = element_text(face = "bold"),
    strip.placement = "bottom",
    panel.spacing.x = unit(20, "pt"),
    panel.spacing.y = unit(15, "pt")
  )
    

ggsave("week_13.png", width = 4, height = 9, dpi = 300)

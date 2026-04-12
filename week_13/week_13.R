library(tidyverse)
library(lubridate)


ocean_temperature <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-31/ocean_temperature.csv')
ocean_temperature_deployments <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-31/ocean_temperature_deployments.csv')

oc_temp <- ocean_temperature |>
  mutate(
    month = month(date, label = TRUE),
    year = year(date)
  ) |>
  group_by(month, year, sensor_depth_at_low_tide_m) |>
  summarise(
    mean_temp = mean(mean_temperature_degree_c, na.rm = TRUE)
  ) |> 
  ungroup() |>
  drop_na()

oc_temp |>
  count(month)
  

last_Dec <- oc_temp |>
  filter(month == "Dec") |>
  mutate(year = year - 1,
         month = "last_Dec")


next_Jan <- oc_temp |>
  filter(month == "Jan") |>
  mutate(year = year + 1,
         month = "next_Jan")

bind_rows(last_Dec, oc_temp) |>
  mutate(
    month = factor(month, levels = c("last_Dec", month.abb, "next_Jan")),
    month_numeric = as.numeric(month) - 1
  ) |>
  ggplot(
    aes(x = month_numeric, y = mean_temp, color = year, group = year)
  ) + 
  geom_line() + 
  facet_wrap(vars(sensor_depth_at_low_tide_m)) + 
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_color_viridis_c() +
  # coord_polar() +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  )


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

bind_rows(last_Dec, oc_temp) |>
  arrange(year, month) |>
  group_by(sensor_depth_at_low_tide_m) |>
  mutate(
    diff_month = month - lag(month),
    seq_group = cumsum(is.na(diff_month) | diff_month > 1)
  ) |>
  ungroup() |>
  mutate(
    month_abb = factor(month, levels = 0:13, labels = c("last_Dec", month.abb, "next_Jan"))
  ) |>
  ggplot(
    aes(x = month, y = mean_temp, color = as.factor(year), group = interaction(year,seq_group))
  ) + 
  geom_line() +
  facet_wrap(vars(sensor_depth_at_low_tide_m)) + 
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_color_viridis_d() +
  coord_polar() +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  )
  
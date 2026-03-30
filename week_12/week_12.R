library(tidyverse)
library(gganimate)
library(gifski)

tuesdata <- tidytuesdayR::tt_load(2026, week = 12)

pi_digits <- tuesdata$pi_digits

# palette: https://coolors.co/palette/2b2d42-8d99ae-edf2f4-ef233c-d90429
bk_color <- "#2b2d42"
pr_color <- "#edf2f4"
hl_color <- "#ef233c"


pi_digits |>
  head(300) |>
  group_by(digit) |>
  summarise(count = n()) |>
  ungroup() |>
  mutate(prop = round(count / sum(count), 3)) |>
  ggplot(aes(x = digit, y = count)) +
  geom_col(
    fill = pr_color
  ) + 
  geom_text(
    aes(label = count),
    position = position_stack(vjust = 0.5),
    color = bk_color
  ) + 
  scale_x_continuous(breaks = 0:9, limits = c(-0.5,9.5)) +
  labs(
    title = "Distribution of first n digits of π",
    caption = "Data: TidyTuesday week 12"
  ) + 
  theme_minimal(base_family = "Google Sans") +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0, margin = margin(t = 15), color = pr_color),
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(color = pr_color, face = "bold", size = 18),
    plot.background = element_rect(fill = bk_color),
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(color = pr_color, size = 17)
  )


## Animación
digits_seq <- c(1:200, seq(200, 1000, by=100),seq(2000, 101000, by = 1000))

pi_acum_dist <- map(digits_seq, \(n){
  pi_digits |>
    head(n) |>
    count(digit, name = "conteo") |>
    mutate(num_digits = n)
}) |>
  list_rbind()

  
## Evitar usar notación científica
options(scipen = 999)

pi_anim <- pi_acum_dist  |>
  # filter(num_digits == 1) |>
  ggplot(aes(x = digit, y = conteo)) +
  geom_col(
    fill = pr_color,
    aes(group = 1)
  ) + 
  geom_text(
    aes(label = conteo, group = 1),
    position = position_stack(vjust = 0.5),
    color = bk_color
  ) + 
  scale_x_continuous(breaks = 0:9, limits = c(-0.5,9.5)) +
  labs(
    title = "Distribution of first {closest_state} digits of π",
    caption = "Data: TidyTuesday week 12"
  ) + 
  transition_states(num_digits, transition_length = 0) +
  view_follow(fixed_x = TRUE) +
  theme_minimal(base_family = "Google Sans") +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0, margin = margin(t = 15), color = pr_color),
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(color = pr_color, face = "bold", size = 18),
    plot.background = element_rect(fill = bk_color),
    panel.grid = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(color = pr_color, size = 17)
  )


anim_render <- animate(
  pi_anim,
  duration = 10,
  end_pause = 10
  )
  
anim_save("./anim_03.gif", anim_render)

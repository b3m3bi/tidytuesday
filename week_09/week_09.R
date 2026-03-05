library(tidyverse)
library(lubridate)
library(ggimage)
library(ggtext)
library(showtext)
library(patchwork)
library(glue)

tuesdata <- tidytuesdayR::tt_load(2026, week = 9)

clutch_size_cleaned <- tuesdata$clutch_size_cleaned
tortoise_body_condition_cleaned <- tuesdata$tortoise_body_condition_cleaned

clutch_size_cleaned <- clutch_size_cleaned |>
  mutate(
    locality = factor(locality, levels = c("Konjsko", "Beach", "Plateau")),
  )

tortoise_body_condition_cleaned <- tortoise_body_condition_cleaned |>
  mutate(
    locality = factor(locality, levels = c("Konjsko", "Beach", "Plateau")),
    individual = as.numeric(individual)
  )


clutch_size_cleaned |>
  ggplot(
    aes(x = age)
  ) + 
  geom_histogram() + 
  facet_grid(vars(locality))

tortoise_sex_ratios <- tortoise_body_condition_cleaned |>
  group_by(locality, year, sex) |>
  summarise(
    n = n()
  ) |>
  pivot_wider(
    names_from = sex,
    values_from = n
  ) |>
  mutate(
    sex_ratio = m / f
  )


tortoise_sex_ratios |>
  ggplot(aes(x = year, y = sex_ratio )) + 
  geom_line() +
  facet_grid(vars(locality)) +
  labs(
    y = "Sex ratio\n(number of males per female)",
    x = "Year"
  )


tortoise_body_condition_cleaned |>
  group_by(locality, year, sex) |>
  summarise(
    n = n()
  ) |>
  mutate(
    sex = if_else(sex == "f", "female", "male")
  ) |>
  ggplot(aes(x = year, y = n, fill = sex)) + 
  geom_area() +
  facet_grid(vars(locality)) +
  labs(
    x = "Year",
    y = "Number of tortoise",
    fill = "Sex"
  )


tortoise_body_condition_cleaned |>
  group_by(locality, year, sex) |>
  summarise(
    n = n()
  ) |>
  mutate(
    sex = if_else(sex == "f", "female", "male"),
  ) |>
  ungroup() |>
  group_by(locality, year) |>
  mutate(
    percent = n / sum(n) * 100
  ) |>
  ggplot(aes(x = year, y = percent, fill = sex)) + 
  geom_area() +
  geom_hline(aes(yintercept = 50)) + 
  facet_grid(vars(locality)) +
  labs(
    x = "Year",
    y = "Percent of tortoise",
    fill = "Sex"
  )

tortoise_body_condition_cleaned |>
  ggplot(
    aes(x = body_condition_index, y = sex, fill = sex)
  ) +
  geom_violin() + 
  facet_grid(vars(locality))

clutch_size_cleaned |>
  mutate(
    sex = "female",
    sex = factor(sex, levels = c("female", "male"))) |>
  ggplot(
    aes(x = eggs, y = sex , fill = sex)
  ) + 
  geom_violin() + 
  facet_grid(vars(locality))

clutch_size_cleaned |>
  mutate(
    year = year(date)
  ) |>
  group_by(year, locality) |>
  summarise(
    mean_eggs = mean(eggs),
    sum_eggs = sum(eggs)
  ) |>
  ungroup() |>
  ggplot(aes(x = year, y = mean_eggs, color = locality)) + 
  geom_line() 

clutch_size_cleaned |>
  ggplot(aes(x = body_mass_grams, y = eggs, color = locality, size = straight_carapace_length_mm)) +
  geom_point(alpha = 0.75) + 
  labs(
    x = "Body mass (g)",
    y = "Number of eggs",
    color = "Site",
    size = "Caparace length (mm)"
  ) +
  theme_minimal()
  
color_palette <- c("#1446a0", "#db3069", "#e0b90cff")

font_add_google(
  "Google Sans"
)
showtext_auto()

p2 <- clutch_size_cleaned |>
  ggplot(
    aes(x = body_mass_grams, y = eggs, color = locality),
  ) +
  # geom_point(size = 5) +
  geom_point(alpha = 0,) +
  geom_image(
    image = "./imgs/turtle.png",
    ) +
  guides(
  color = guide_legend(override.aes = list(size = 5, alpha = 1))
  ) +
  labs(
    x = "Body mass (g)",
    y = "Number of eggs",
    color = "Site"
  ) +
  scale_color_manual(values = color_palette) + 
  theme_minimal() +
  theme(
    text = element_text(family = "Google Sans"),
    legend.position = "none",
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    # plot.margin = unit(c(10,10,10,10), 'pt')
  )
  


data <- tortoise_body_condition_cleaned |>
  group_by(locality, year, sex) |>
  summarise(
    n = n()
  ) |>
  mutate(
    sex = if_else(sex == "f", "female", "male"),
    sex_symb = if_else(sex == "female", "♀", "♂"),
    sex_loc = paste0(sex, "_", locality),
    sex_loc = factor(sex_loc, levels = c("female_Konjsko", "male_Konjsko", "female_Beach", "male_Beach", "female_Plateau", "male_Plateau")),
    color = case_when(
      locality == "Konjsko" ~ "#1446a0",
      locality == "Beach" ~ "#db3069",
      locality == "Plateau" ~ "#e0b90cff"
    ),
    loc_style = glue("<b style='color:{color}'>{locality}</b>")
  ) |>
  ungroup() |>
  group_by(locality, year) |>
  mutate(
    percent = n / sum(n) * 100
  )

p1 <- data |>
  filter(year >= 2010) |>
  ggplot(aes(x = year, y = percent, fill = sex_loc)) + 
  geom_area() +
  geom_text(
    data = data |> filter(year == max(data$year)),
    aes(label = sex_symb, color = sex_loc), 
    position = position_stack(vjust = 0.5),
    hjust = -0.2,
    size = 6
    ) +
  geom_hline(aes(yintercept = 50), color = "white", linetype=3) + 
  facet_wrap(vars(loc_style), ncol = 1, strip.position = "top") +
  scale_fill_manual(values = c("#1446a0", "#588beaff","#db3069", "#ec92b0ff","#e0b90cff", "#f8e17bff")) +
  scale_color_manual(values = c("#1446a0", "#588beaff","#db3069", "#ec92b0ff","#e0b90cff", "#f8e17bff")) +
  scale_y_continuous(breaks = c(0, 50, 100), labels = c(0, 50, 100)) + 
  scale_x_continuous(expand = expansion(add = c(0, 1))) +
  labs(
    x = "Year",
    y = "Percent of tortoise",
    fill = "Sex"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Google Sans"),
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_textbox(size = 12),
    panel.grid.minor = element_blank(),
  )


plot <- p1 / p2 + 
  plot_layout(widths = unit(c(10, 1), 'cm'), heights = unit(c(12, 5), 'cm')) +
  plot_annotation(
    title = "A tortoise population at risk of extinction",
    subtitle = "Sex ratios of Hermann's tortoise populations on Golem Grad Island in North Macedonia (<strong><span style='color:#db3069;'>Beach</span></strong> and <strong><span style='color:#e0b90cff'>Plateu</span></strong>) show a strong male bias compared with other populations (<strong><span style='color: #1446a0'>Konjsko</span></strong>). In 2023, the Plateu population reached 16.8 males per female. Male harassment reduces female size and clutch size, pushing the population toward an extinction vortex.",
    caption = "Data: Arsovski, et al. (2026). TidyTuesday week 9",
    theme = theme(
      text = element_text(family = "Google Sans"),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = element_text(margin = margin(b = 20), face = "bold"),
      plot.subtitle = element_textbox_simple(margin = margin(b = 15), lineheight = 1.5, size = 9),
      plot.caption = element_text(margin = margin(t = 15), hjust = 0, color = "gray45"),
      legend.position = "none",
      plot.margin = unit(c(10,10,10,10), 'pt')
    )
  )
  
plot

library(ragg)
options(
  device = ragg::agg_png,
  ragg.default.units = "cm"
)

ggsave("figura.png", plot, dpi = 300)

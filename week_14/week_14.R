library(tidyverse)

sysfonts::font_add(
  family = "fa7",
  regular = "../fonts/fontawesome-free-7.2.0-desktop/otfs/Font Awesome 7 Free-Solid-900.otf"
)
sysfonts::font_add("arial", "arial.ttf", bold = "Arial_Bold.ttf")
sysfonts::font_add("google sans", "GoogleSans-Regular.ttf", bold = "GoogleSans-Bold.ttf")
sysfonts::font_add("roboto", "Roboto-Regular.ttf", bold = "Roboto-Bold.ttf")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 96)


# tuesdata <- tidytuesdayR::tt_load(2026, week = 14)
# repairs <- tuesdata$repairs
# repairs_text <- tuesdata$repairs_text

repairs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-07/repairs.csv')

plot_data <- repairs |> 
  group_by(category) |>
  count(repaired) |>
  filter(repaired != "ja") |>
  mutate(
    percent = n / sum(n) * 100,
    percent_lab = paste0(round(percent, 0), ""),
    sort_repaired = percent[repaired == "yes"],
    order_repaired = rank(sort_repaired),
    repaired = factor(repaired, levels = c("yes", "half", "no"), labels = c("Repaired", "Half repaired", "Not repaired"))
  ) |>
  drop_na()

icons <- tibble(
  category = plot_data |>
    select(category) |> unique() |> pull(),
  icon_code = c(
    "f206",
    "f34e",
    "e4e5",
    "e163",
    "f4b8",
    "f517",
    "f2e7",
    "f3a5",
    "f535",
    "f553",
    "f5df",
    "f0ad",
    "f63b",
    "f135"
  )
)
  
color_palette <- c("#8ac926", "#ffca3a", "#ff595e")
bk_color <- "#fafafa"
  
plot_data <- plot_data |>
  left_join(icons) |>
  mutate(
    category_icon = glue::glue("<span style='font-family:fa7;'>&#x{icon_code};</span>")
  )

plot_data |>
  ggplot(
    aes( 
      y = fct_reorder(category, sort_repaired), 
      x = percent
      )
    ) +
  geom_col(
    aes(fill = repaired),
    width = 0.85
    ) +
  geom_text(
    aes(label = percent_lab, group = repaired), 
    position = position_stack(vjust = 0.5),
    size = 3.5,
    color = bk_color,
    fontface = "bold",
    family = "google sans"
    ) +
  ggtext::geom_richtext(
    aes(x = -6, y = , label = category_icon ), 
    color = "black",
    fill = bk_color,
    label.color = bk_color
  ) + 
  labs(
    x ="",
    y ="",
    title = "Repair Success Rates Vary Widely by Item Type",
    subtitle = "Repair Cafés connect volunteer fixers with people seeking to repair broken items, with higher success rates for non-electronic goods.",
    caption = "Data: Repair Monitor. Tidytuesday 2026 week 13."
    ) +
  scale_fill_manual(
    values = color_palette,
    guide = guide_legend(reverse = TRUE)
    ) +
  scale_x_continuous(breaks = c(0, 100), labels = scales::label_percent(scale = 1)) +
  theme(
    text = element_text(family = "google sans"),
    legend.position = "top",
    legend.background = element_rect(fill = bk_color),
    panel.background = element_blank(),
    axis.text.y = element_text(colour = "black"),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.title = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", margin = margin(b = 10), size = 17),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0, margin = margin(t = 10), color = "gray45"),
    plot.subtitle = ggtext::element_markdown(),
    plot.margin = margin(t =10, l = 10, b = 10, r = 10),
    plot.background = element_rect(fill = bk_color)
  )
  
showtext::showtext_opts(dpi = 96)
ggsave("week_14.png", dpi = 300, width = 8, height = 6)

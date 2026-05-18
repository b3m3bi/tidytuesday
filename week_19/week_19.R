library(tidyverse)
library(igraph)
library(sf)
library(patchwork)

sysfonts::font_add_google("Host Grotesk", family = "host grotesk")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 96)

bk_color <- "#edf2f4"
pm_color <- "#2b2d42"
sc_color <- "#8d99ae"
hl_color <- "#ef233c"


tuesdata <- tidytuesdayR::tt_load(2026, week = 19)

cities <- tuesdata$cities
links <- tuesdata$links


G <- graph_from_data_frame(
  d = links,
  directed = TRUE,
  vertices = cities
)

degrees <- tibble(
  degree = as.vector(degree(G)),
  city = cities$name,
  id = cities$id
  )

annot <- degrees |>
  arrange(desc(degree)) |>
  head(n = 10) |>
  mutate(
    y = c(100, 250, 100, 250, 400, 550, 700, 850, 250, 100),
    xlabel = c(109, 105, 73, 72, 63, 65.5, 63.5, 65.5, 41.5, 40),
    xstep = c(rep(3, 8), rep(-3, 2)),
    hjust = c(rep(0, 8), rep(1, 2)),
    label = paste0(city," (",degree,")")
  )

p1 <- degrees |>
  ggplot(aes(x = degree)) +
  geom_histogram(binwidth = 1, color = bk_color, fill = sc_color) +
  ggrepel::geom_text_repel(
    data = annot, 
    aes(x = degree + xstep, y = y, label = label, hjust = hjust),
    color = pm_color,
    size = 3,
    bg.colour = "white", bg.r = .15, force = 0
    ) +
  geom_segment(
    data = annot, 
    aes(x = degree, xend = degree, y = 0, yend = y),
    color = pm_color
  ) +
  geom_segment(
    data = annot, 
    aes(x = degree, xend = degree + xstep , y = y, yend = y),
    color = pm_color
  ) +
  geom_point(
    data = annot,
    aes(x = degree, y = 0),
    color = "white",
    size = 3
  ) +
  geom_point(
    data = annot,
    aes(x = degree, y = 0),
    color = hl_color
  ) +
  labs(
    x = "Number of twin cities\n(degree)",
    y = "Number of cities\n(frequency)",
    ) +
  scale_x_continuous(limits = c(0, 135), expand = c(0,0)) +
  scale_y_continuous(expand = c(0,40)) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill = bk_color),
    text = element_text(family = "host grotesk", color = pm_color),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold"),
    plot.subtitle = ggtext::element_textbox_simple(),
    plot.caption.position = "plot",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "white"),
    axis.title.x = element_text(margin = margin(t=5)),
    axis.title.y = element_text(margin = margin(r=10)),
    plot.margin = margin(t=20, l=20, b=20, r=30)
  )

links_plot <- links |>
  left_join(
    cities |> select(id, lng, lat),
    by = join_by(source == id)
  ) |>
  rename(
    s_lng = lng,
    s_lat = lat
  ) |>
  left_join(
    cities |> select(id, lng, lat),
    by = join_by(target == id)
  ) |> 
  rename(
    t_lng = lng,
    t_lat = lat
  )

annot_map <- annot |>
  left_join(
    cities |>
      select(id, name, lng, lat)
    ) |>
  mutate(
    r = row_number(),
    label = paste0(r,".",city)
  )

p2 <- cities |> 
  right_join(degrees, by = join_by(name == city), multiple = "first") |>
  ggplot() +
  geom_curve(
    data = links_plot,
    aes(x = s_lng, xend = t_lng, y = s_lat, yend = t_lat),
    color = pm_color,
    alpha = 0.02,
    curvature = 0.3
  ) +
  geom_point(
    aes(x = lng, y = lat),
    size = 0.5,
    alpha = 0.2,
    color = pm_color
    ) +
  geom_point(
    data = annot_map,
    aes(x = lng, y = lat),
    color = "white",
    size = 2) +
  geom_point(
    data = annot_map,
    aes(x = lng, y = lat),
    color = hl_color,
    size = 1.5
  ) +
  ggrepel::geom_text_repel(
    data = annot_map,
    aes(x = lng, y = lat, label = label),
    bg.colour = "white", bg.r = .2,
    size = 3.5,
    color = pm_color
  ) +
  coord_quickmap(
    clip = "off",
    xlim = c(-120, 190)
  ) +
  theme_void() +
  theme(
    text = element_text(family = "host grotesk"),
    legend.position = "none",
    plot.background = element_rect(fill = bk_color)
  )

multiplot <- (p2 / p1) +
  plot_layout(heights = c(1,0.75)) +
  plot_annotation(
    title = 'The Global Twin Cities Network',
    subtitle = 'Twin cities are legal agreements between cities for promoting commertial and cultural ties. Spatially localized network with the top 10 highest-degree cities highlighted, alongside the network’s power-law degree distribution.',
    caption = 'Data: Twin cities Explorer. Wikipedia - TidyTuesday week 19.',
    theme = theme(
      plot.background = element_rect(fill = bk_color),
      text = element_text(family = "host grotesk", color = pm_color, size = 14),
      plot.subtitle = ggtext::element_textbox_simple(margin = margin(b = 15, t = 5), size = 12),
      plot.title = element_text(face = 'bold', margin = margin(b=10, t = 10)),
      plot.caption.position = "plot",
      plot.caption = element_text(hjust = 0, margin = margin(t= 5, b = 15))
    )
  )


showtext::showtext_opts(dpi = 300)
ggsave("week_19.png", multiplot, dpi = 300, width = 6, height = 8, units = "in")

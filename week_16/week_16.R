library(tidyverse)

sysfonts::font_add_google("Host Grotesk", family = "host grotesk")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 96)


tuesdata <- tidytuesdayR::tt_load(2026, week = 16)

financing_schemes <- tuesdata$financing_schemes
health_spending <- tuesdata$health_spending
spending_purpose <- tuesdata$spending_purpose


regions <- read_csv("https://ourworldindata.org/grapher/continents-according-to-our-world-in-data.csv?v=1&csvType=full&useColumnShortNames=true")

latam <- c( "Argentina","Bolivia",
            "Brazil","Chile",
            "Colombia","Costa Rica",
            "Cuba","Dominican Republic",
            "Ecuador","El Salvador",
            "Guatemala","Honduras",
            "Mexico","Nicaragua",
            "Panama","Paraguay",
            "Peru","Uruguay","Venezuela")

regions |> filter(entity %in% latam) |> select(entity, code) |> write_csv("latam.csv")

mygrid <- data.frame(
  name = c("Mexico", "Cuba", "Guatemala", "Dominican Republic", "Honduras", "El Salvador", "Nicaragua", "Costa Rica", "Panama", "Venezuela", "Colombia", "Ecuador", "Brazil", "Bolivia", "Paraguay", "Peru", "Uruguay", "Chile", "Argentina"),
  code = c("MEX", "CUB", "GTM", "DOM", "HND", "SLV", "NIC", "CRI", "PAN", "VEN", "COL", "ECU", "BRA", "BOL", "PRY", "PER", "URY", "CHL", "ARG"),
  row = c(1, 1, 2, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 6, 7, 7, 7, 8, 8),
  col = c(2, 4, 2, 5, 3, 2, 3, 3, 4, 5, 4, 3, 5, 4, 4, 3, 5, 3, 4),
  stringsAsFactors = FALSE
)
geofacet::grid_preview(mygrid)


rename <- tribble(
  ~from, ~to,
  "Bolivia (Plurinational State of)", "Bolivia",
  "Venezuela (Bolivarian Republic of)", "Venezuela"
)

names_es <- tribble(
  ~en, ~es,
  "Argentina", "Argentina",
  "Bolivia", "Bolivia",
  "Brazil", "Brasil",
  "Chile", "Chile",
  "Colombia", "Colombia",
  "Costa Rica", "Costa Rica",
  "Cuba", "Cuba",
  "Dominican Republic", "República Dominicana",
  "Ecuador", "Ecuador",
  "El Salvador", "El Salvador",
  "Guatemala", "Guatemala",
  "Honduras", "Honduras",
  "Mexico", "México",
  "Nicaragua", "Nicaragua",
  "Panama", "Panamá",
  "Paraguay", "Paraguay",
  "Peru", "Perú",
  "Uruguay", "Uruguay",
  "Venezuela", "Venezuela"
)


plot_data <- financing_schemes |>
  select(country_name, year, indicator_code, value) |>
  group_by(country_name, indicator_code) |>
  mutate(
    value_lab = paste0(round(value, 1), "%"),
    first_value = value[which.min(year)],
    lastest_value = value[which.max(year)],
    mean_value = round(mean(value), 1),
    mean_value_lab = paste0(mean_value, "%"),
    change = round(lastest_value - first_value,1),
    change_in_period = if_else(
      change < 0, "", "+"
    ),
    change_lab = paste0(change_in_period, change, "%")
  ) |>
  mutate(
    country_name = recode_values(
      country_name, 
      from = rename$from, 
      to = rename$to,
      default = country_name
    )
  ) |>
  filter(country_name %in% latam) |> 
  filter(indicator_code == "hf3_che") |>
  left_join(names_es, by = join_by(country_name == en)) |>
  ungroup()
  
color_palette <- c("#6a994e", "#bc4749")

title <- "Gasto directo de los pacientes en salud en Latinoamérica"
subtitle <- "Porcentaje del gasto total en salud que es pagado directamente por los pacientes. Durante el periodo de 2000 a 2023 en 15 países latinoamericanos ha habido una <strong><span style='color:#6a994e'>reducción</span></strong> porcentual en los gastos en salud pagados por directamente por los pacientes, mientras que en 4 países ha habido un <strong><span style='color:#bc4749'>incremento</span></strong>."
caption <- "Datos: Global Health Expenditure Database. TidyTuesday semana 16."

plot_data |>
  filter(year %in% c(2000, 2023)) |>
  ggplot(aes(x = year, y = value, group = es, color = change_in_period)) + 
  geom_line(
    linewidth = 0.8
  ) + 
  geom_point(
    size = 2
  ) +
  gghighlight::gghighlight(
    use_direct_label = FALSE,
    unhighlighted_params = aes(colour = "gray90")
    ) + 
  geom_text(
    data = plot_data |> filter(year == 2000),
    aes(label = value_lab),
    position = position_nudge(x = -9),
    size = 7.5,
    size.unit = "pt",
  ) + 
  geom_text(
    data = plot_data |> filter(year == 2023),
    aes(label = value_lab),
    position = position_nudge(x = 9),
    size = 7.5,
    size.unit = "pt"
  ) + 
  expand_limits(x = c(1985, 2038), y = c(10, 70)) +
  geom_text(
    data = plot_data |> filter(year == 2023),
    aes(label = change_lab),
    x = 2011.5,
    y = 68,
    size = 10, 
    size.unit = "pt",
    fontface = "bold"
  ) +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption
  ) +
  facet_wrap(vars(fct_reorder(es, lastest_value))) +
  scale_x_continuous(breaks = c(2000, 2023)) +
  scale_color_manual(values = color_palette) +
  theme_minimal() +
  theme(
    text = element_text(family = "host grotesk"),
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 10),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 10)),
    plot.subtitle = ggtext::element_textbox_simple(margin = margin(b = 20), color = "gray30"),
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0, color = "gray30", margin = margin(t = 20)),
    plot.margin = margin(t = 15, b = 15, r = 15, l = 15),
    plot.background = element_rect(fill = "#fafafa")
  )

showtext::showtext_opts(dpi = 300)
ggsave("week_16.png", dpi = 300, width = 8, height = 7)


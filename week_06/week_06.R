library(tidyverse)
library(showtext)

font_add_google("Libre Franklin", "franklin")
font_add_google("Gelasio", "gelasio")
# showtext_opts(dpi = 300)
# showtext_auto(enable = FALSE)


tuesdata <- tidytuesdayR::tt_load(2026, week = 6)

schedule <- tuesdata$schedule

# Para qué deporte hay más eventos?
schedule |>
  group_by(discipline_name) |>
  summarise(
    number_of_events = n()
  ) |>
  arrange(number_of_events) |>
  ggplot(
    aes(x = number_of_events, y = fct_reorder(discipline_name, number_of_events))
  ) +
  geom_col() +
  labs(
    title = "Number of registered events by sport",
    x = "Number of events",
    y = ""
  )


schedule |> 
  filter(discipline_name == "Bobsleigh") |>
  view()

# ¿Cuántos eventos hay de hombres y mujeres y mixtos?
schedule_gender <- schedule |>
  mutate(
    gender = 
      if_else(
        discipline_name == "Nordic Combined", 
        "Men",
        str_extract(event_description, "Men|Women|Mixed|-man|-woman")
      ),
    gender = case_match(
      gender,
      "-man" ~ "Men",
      "-woman" ~ "Women",
      NA ~ "Mixed",
      .default = gender
    ),
    gender = factor(gender, levels = c("Men", "Women", "Mixed"))
    ) |>
  group_by(discipline_name, gender) |>
  summarise(
    number_of_events = n()
  ) |>
  ungroup() |>
  complete(
    discipline_name, gender,
    fill = list(number_of_events = 0)
  ) |>
  group_by(discipline_name) |>
  mutate(
    total_events = sum(number_of_events),
    prop_events = (number_of_events / total_events) * 100,
    prop_lab = paste0(round(prop_events, 0), "%"),
    prop_women = prop_events[gender == "Women"],
    prop_men = prop_events[gender == "Men"],
    prop_mixed = if_else(discipline_name == "Nordic Combined", -1, prop_events[gender == "Mixed"])
  ) |>
  filter(prop_events != 0)


schedule_gender |>
  ggplot(
    aes(
      x = prop_events,
      y = fct_reorder(discipline_name, prop_mixed),
      fill = gender,
      label = prop_lab
    ) 
  ) +
  geom_col() +
  geom_text(position = position_stack(vjust = 0.5) , size = 2.75) +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(
    x = NULL,
    y = NULL,
    title = "Percentage of Events by Gender at the\nWinter Olympics Games",
    subtitle = "The Milano-Cortina 2026 Winter Olympic Games feature a total of 1866 events: \n852 men's events, 796 women's events and 218 mixed events. Nordic Combined\nis the only sport that exclusively allows men to compete.",
    caption = "Data: Milano-Cortina 2026 Winter Olympics. TidyTuesday Week 6"
  ) +
  scale_fill_manual(
    name = NULL,
    breaks = c("Mixed", "Women", "Men"),
    # values = c("#bee3db", "#faf9f9", "#ffd6ba"),
    # values = c("#dbcbd8", "#f2fdff", "#9ad4d6"),
    values = c("#efcfe3", "#eaf2d7", "#b3dee2"),
    # labels = c("Mixed event", "Women's event", "Men's event")
  ) +
  theme(
    text = element_text(family = "Arial"),
    legend.position = "top",
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 8, color = "black", margin = margin(r = 8)),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(margin = margin(b = 10), size = 9),
    plot.caption = element_text(hjust = 0, margin = margin(t = 15), size = 8, color = "#999")
  )


ggsave("winter_olympics_events_gender.png", width = 5, height = 6)































schedule_gender |>
  ggplot(
    aes(
      y = number_of_events,
      x = gender,
      fill = gender
    ) 
  ) +
  geom_col() +
  geom_text(
    aes(label = number_of_events),
    vjust = -0.2
  ) + 
  facet_wrap(vars(discipline_name)) +
  labs(
    title = "Number of schedulled events by gender",
    subtitle = "In the Winter Olympics there are a total of 1866 schedulled sport's events. 852 are of men, 796 of women and 218 are mixed events. Nordic Combined is the only sport that only allows men to compete.",
    y = "Number of events"
  ) +
  lims(
    y = c(0, 200)
  ) +
  theme_bw()




schedule_gender |>
  group_by(gender) |>
  summarise(
    total_number_of_events = sum(number_of_events)
  )




schedule_gender |>
  group_by(gender) |>
  summarise(
    total_number_of_events = sum(number_of_events)
  )

sum(schedule_gender$number_of_events)


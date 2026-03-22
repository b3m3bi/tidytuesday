library(tidyverse)
library(ggtext)
library(ggbeeswarm)
library(paletteer)

tuesdata <- tidytuesdayR::tt_load(2026, week = 10)

absolute_judgements <- tuesdata$absolute_judgements
pairwise_comparisons <- tuesdata$pairwise_comparisons
respondent_metadata <- tuesdata$respondent_metadata

plot_data <- absolute_judgements |> 
  left_join(respondent_metadata) |>
  drop_na(age_band) |>
  mutate(
    age_band = factor(age_band, levels = c(
      "Under 18",
      "18-24",
      "25-34",
      "35-44",
      "45-54",
      "55-64",
      "65-74",
      "75+"
    ))
  )

all_ages_mean_prob <- plot_data |>
  group_by(term) |>
  summarise(mean_prob_all_ages = mean(probability)) 


plot_data |>
  group_by(age_band, term) |>
  summarise(
    mean_prob = mean(probability),
    min_prob = min(probability),
    max_prob = max(probability)
  ) |>
  left_join(all_ages_mean_prob)|>
  ggplot(
    aes(y = age_band, x = mean_prob, color = age_band)
  ) + 
  geom_point() +
  facet_wrap(vars(fct_reorder(term, mean_prob_all_ages)), ncol = 1, strip.position = "left") +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.text.y.left = element_text(angle = 0)
  )


####### 

mean_probs <- plot_data |>
  group_by(term) |>
  summarise(
    mean_prob_term = mean(probability)
  )


optimism_data <- plot_data |> 
  group_by(term, age_band) |> 
  summarise(
    mean_prob_age_term = mean(probability)
  ) |> 
  ungroup() |>
  left_join(mean_probs) |>
  mutate(
    optimism_term_age = mean_prob_age_term - mean_prob_term
  )

optimism_data_age <- optimism_data |>
  group_by(age_band) |>
  summarise(
    optimism_age_band = mean(optimism_term_age)
  )
  
ggplot() +
  geom_beeswarm(
    data = optimism_data,
    aes(x = optimism_term_age, y = age_band, color = age_band)
  ) +
  geom_point(
    data = optimism_data_age,
    aes(x = optimism_age_band, y = age_band),
    shape = 24,
    color = "black",
    fill = "white",
    size = 2,
  ) +
  geom_vline(xintercept = 0, linetype = 2) +
  annotate("text", label = "Each dot is a\nprobability phrase", x = -6, y = 5, size = 3.1, color = "gray30",lineheight = 0.75) +
  annotate("curve", x = -6, y = 5.5, xend = -3, yend = 6, arrow = arrow(length = unit(2,"mm")), color = "gray30", curvature = -.2) +
  annotate("text", label = "Mean optimism\nof group", x = 6.1, y = 2.7, size = 3.1, color = "gray30", lineheight = 0.75) +
  annotate("curve", x = 4.5, y = 2.5, xend = 3.2, yend = 1.3, arrow = arrow(length = unit(2,"mm")), color = "gray30", curvature = .2) +
  scale_x_continuous(limits = c(-10, 10)) +
  scale_color_paletteer_d("ggsci::teal_material", direction = -1) +
  labs(
    title = "Younger people tend to be more optimistic",
    subtitle = "Participants in an online quiz assigned probabilities to 19 probability phrases (e.g., 'Highly Unlikely', 'Little Chance', 'Almost Certain'). Younger respondents tend to be more optimistic as they generally gave higher probabilities to the phrases.",
    x = "Optimism¹",
    y = "Age group",
    caption = "1: Optimism is measured as the difference between a term's probability for an age group and its overall average probability.<br>*Data*: CAPphrase database. TidyTuesday week 10."
  ) +
  theme_minimal(base_family = "Google Sans") + 
  theme(
    legend.position = "none",
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_textbox_simple(hjust = 0, color = "gray30", margin = margin(t = 15)),
    plot.title = element_text(face = "bold", size = 14, margin = margin(b = 15)),
    plot.subtitle = element_textbox_simple(margin = margin(b = 15)),
    plot.margin = unit(c(10, 10, 10, 10), 'pt'),
    panel.grid.minor = element_blank()
  )

ggsave(
  "week_10.png", 
  width = 6,
  height = 5,
  dpi = 300
  )

library(tidyverse)
library(tidytext)
library(ggwordcloud)
library(lubridate)
library(igraph)
library(paletteer)

tuesdata <- tidytuesdayR::tt_load(2026, week = 8)
sfi_grants <- tuesdata$sfi_grants

sfi_grants |> 
  group_by(research_body) |> 
  summarise(
    number_of_grants = n()
  )  |> 
  arrange(desc(number_of_grants)) |>
  head(n = 15) |>
  ggplot(
    aes(x = number_of_grants, y = fct_reorder(research_body, number_of_grants))
  ) +
  geom_col() +
  geom_text(aes(label = number_of_grants), size = 3, position = position_stack(), hjust = -0.2) + 
  labs(
    title = "Number of grants by Institution"
  )


data("stop_words")

sfi_words <- sfi_grants |>
  select(proposal_title, start_date) |> 
  unnest_tokens(word, proposal_title) |>
  anti_join(stop_words)
  
sfi_words |> 
  count(word, sort=TRUE) |>
  head(50) |>
  ggplot(
    aes(label = word, size = n, color = n)
  ) + 
  geom_text_wordcloud() + 
  scale_size_area(max_size = 8) +
  theme_minimal()

sfi_words |>
  mutate(year = year(start_date)) |>
  filter(year > 2010) |>
  group_by(year) |>
  count(word, sort = TRUE) |>
  ungroup() |>
  group_by(year) |>
  top_n(5, n) |>
  arrange(year, desc(n)) |> 
  ggplot(
    aes(x = year, y = n, label = word) 
  ) +
  geom_text()


sfi_grants |>
  select(start_date, end_date, current_total_commitment) |> 
  mutate(
    duration = time_length(interval(start_date, end_date), "years"),
    duration_floor = floor(duration)
  ) |> 
  group_by(duration_floor) |> 
  summarise(
    total_grant = sum(current_total_commitment)
  ) |> 
  ggplot(aes(x = duration_floor, y = total_grant)) + 
  geom_point() +
  geom_line()
   

sfi_grants |>
  select(start_date, proposal_title, current_total_commitment) |>
  mutate(year = year(start_date)) |>
  arrange(desc(current_total_commitment)) |>
  group_by(year) |> 
  top_n(1, current_total_commitment) |> view()


sfi_words_links <- sfi_grants |>
  select(proposal_id, proposal_title) |> 
  group_by(proposal_id) |> 
  unnest_tokens(word, proposal_title) |>
  anti_join(stop_words) |>
  group_by(proposal_id) |> 
  summarise(
    title_no_connectors = str_c(word, collapse = " "),
    .groups = "drop"
  )

sfi_words_links <- sfi_words_links |>
  unnest_tokens(
    output = bigrama,
    input = title_no_connectors,
    token = "ngrams",
    n = 2
  ) |> 
  separate(bigrama, into = c("word1", "word2"), sep = " ")

sfi_word_count <- sfi_words_links |>
  group_by(word1) |>
  count(word1,  sort= TRUE)

sfi_word_count |> head(50)

G <- sfi_words_links |>
  semi_join(head(sfi_word_count, 10), by = "word1") |>
  graph_from_data_frame()

# sfi_words_links <- sfi_words_links |> 
#   inner_join(sfi_words_links, by="proposal_id", suffix = c("1", "2"), relationship = 'many-to-many') |> 
#   filter(word1 < word2) |>
#   select(word1, word2)

plot(G)




sfi_words <- sfi_grants |>
  select(proposal_title, start_date) |> 
  unnest_tokens(word, proposal_title) |>
  anti_join(stop_words)

sfi_words_period <- sfi_words |> 
  mutate(
    year = year(start_date)
  ) |>
  # select(year) |> unique() |> print(n = 100)
  mutate(
    period = case_when(
      year >= 2001 & year <= 2005 ~ "2001-2005",
      year >= 2006 & year <= 2010 ~ "2006-2010",
      year >= 2011 & year <= 2015 ~ "2011-2015",
      year >= 2016 & year <= 2020 ~ "2016-2020",
      year >= 2021 & year <= 2025 ~ "2021-2025",
    )) |>
  group_by(period) |>
  count(word, sort=TRUE) |>
  top_n(20, n)

sfi_words_period |>
  ggplot(
    aes(label = word, size = n, color = n)
  ) + 
  facet_wrap(~period, ncol = 1, strip.position = "right") + 
  geom_text_wordcloud() + 
  scale_size_area(max_size = 6) +
  labs(
    title = "Most common words used in proposal titles granted\nby the Science Foundation Ireland",
    caption = "Data: Science Foundation Ireland. TidyTuesday week 8."
  ) + 
  scale_colour_paletteer_c("grDevices::Burg", -1) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(face = "bold", size = 12, margin = margin(b = 15)),
    plot.caption = element_text(hjust = 0, margin = margin(t = 10), color = "gray"),
    strip.text = element_text(face = "italic")
  )

ggsave("grant_titles.png", width = 5, height = 7)








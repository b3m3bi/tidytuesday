library(ellmer)
library(tidyverse)
library(lubridate)


tuesdata <- tidytuesdayR::tt_load(2026, week = 8)
sfi_grants <- tuesdata$sfi_grants

classifications <- c(
  "Biological and Medical Sciences",
  "Physical and Chemical Sciences",
  "Engineering and Technology",
  "Computer Science and Mathematics",
  "Environmental and Energy Sciences",
  "Agriculture and Food Sciences",
  "Social Sciences",
  "Interdisciplinary Research"
)

classifier <- chat_openai(
  system_prompt = "
  You are an expert research classification assistant. Your task is to classify scientific research proposal titles into one predefined category.
  
  **Instructions**
  
  You will be given a research proposal title. Analyze its scientific domain and assign it to exactly ONE of the following categories:
  - Biological and Medical Sciences
  - Physical and Chemical Sciences
  - Engineering and Technology
  - Computer Science and Mathematics
  - Environmental and Energy Sciences
  - Agriculture and Food Sciences
  - Social Sciences
  - Interdisciplinary Research
  "
)

title_classification <- parallel_chat_structured(
  classifier, 
  interpolate("{{title}}", title = sfi_grants$proposal_title), 
  type = type_string()
)

# write_csv(as.data.frame(title_classification), "title_classification_openai.csv")
unclassified <- sfi_grants |> filter(!title_classification %in% classifications)

title_classification_2 <-  parallel_chat_structured(
  classifier, 
  interpolate("{{title}}", title = unclassified$proposal_title), 
  type = type_string()
)
#write_csv(as.data.frame(title_classification_2), "title_classification_2_openai.csv")


sfi_grants <- sfi_grants |> add_column(title_classification)

unclassified |> add_column(title_classification_2)

sfi_grants <- sfi_grants |> filter(title_classification %in% classifications) |> bind_rows(unclassified)



classified_grants <- sfi_grants |> 
  filter(title_classification %in% classifications) |> 
  select(start_date, title_classification, proposal_title) |>
  mutate(
    year = year(start_date)
  )

classified_grants |> 
  group_by(title_classification, year) |>
  summarise(
    number_proposals = n()
  ) |>
  ungroup()|>
  filter(year <= 2020) |>
  ggplot(aes(x = year, y = number_proposals, color = title_classification)) + 
  geom_line()











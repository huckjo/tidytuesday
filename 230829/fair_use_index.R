library(pacman)
p_load(fuzzyjoin, tidyverse)

tt <- tidytuesdayR::tt_load('2023-08-29')
cases <- tt$fair_use_cases
findings <- tt$fair_use_findings
rm(tt)

################################################################################
# PREPROCESSING

# Fuzzy join data sets on case number and title
findings <- findings |> 
  mutate(case = paste(title, case_number, sep = ", ")) |> 
  select(case, key_facts, issue, holding, title, case_number)

df <- stringdist_left_join(cases, findings, by = "case", max_dist = 1) |> 
  rename(case = case.x) |> select(-case.y)

rm(cases, findings)

# clean up case categories
case_by_category <- df |> 
  mutate(category = strsplit(as.character(categories), "[;,]")) |> 
  unnest(category) |> 
  mutate(category = tolower(category)) |> 
  mutate(category = str_trim(category)) |> 
  mutate(category = case_when(
    str_detect(category, "education") ~ "education/scholarship/research",
    str_detect(category, "film") ~ "film/audiovisual",
    str_detect(category, "internet") ~ "internet/digitization",
    str_detect(category, "parody") ~ "parody/satire",
    str_detect(category, "photograph") ~ "photography",
    TRUE ~ category
  )) |> 
  filter(!grepl('circuit', category))

################################################################################

# Mo
case_by_category |> 
  group_by(category) |> 
  tally() |> 
  ungroup() |> 
  ggplot(aes(x = reorder(category, n), y = n)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Cases",
       title = "Top categories of Fair Use Cases") +
  coord_flip() +
  theme_minimal() 

# Cumulative number of cases by category
case_by_category |> 
  group_by(category, year) |> 
  tally() |> 
  group_by(category) |> 
  arrange(year) |> 
  mutate(n = cumsum(n)) |> 
  ungroup() |> 
  mutate(category = reorder(category, n, FUN = max, decreasing = TRUE)) |> 
  ggplot(aes(x = year, y = n, group = category, color = category)) +
  geom_line(stat = "identity") +
  labs(x = "", y = "", color = "",
       title = "Fair Use ©ases Over the Years",
       subtitle = "Among the judicial cases involving copyright law, most involve the fair \nuse of textual works such as books, articles, and other written content.",
       caption = "Source: U.S. Copyright Fair Use Index | github.com/huckjo") +
  xlim(1950, 2023) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 20, color = "#FFF5E1"),
    plot.subtitle = element_text(size = 14, color = "#FFF5E1"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_text(size = 8, color = "#FFF5E1"),
    plot.background = element_rect(fill = "#3E2723", color = "#3E2723"),
    panel.background = element_rect(fill = "#3E2723"),
    panel.border = element_rect(color = "#FFF5E1", fill = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 10, color = "#FFF5E1"),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(size = 10, color = "#FFF5E1"),
    axis.text.x = element_text(size = 10, color = "#FFF5E1"),
    axis.ticks.x = element_blank(),
    legend.text = element_text(size = 12, color = "#FFF5E1"),
    legend.title = element_text(size = 12, color = "#FFF5E1"),
    plot.margin = margin(.5, .5,.5, .5, "cm")
  )

################################################################################

# Plot yearly trends by outcome
df |> 
  group_by(year, fair_use_found) |> 
  tally() |> 
  ggplot(aes(x = year, y = n, fill = fair_use_found)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Yearly Trends of Fair Use Cases by Outcome",
       x="Year",
       y="Number of Cases",
       fill="Outcome") +
  xlim(1970, 2023) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot jurisdictions by outcome
df |> 
  group_by(jurisdiction, fair_use_found) |> 
  tally() %>%
  ggplot(aes(x=reorder(jurisdiction, n), y=n, fill=fair_use_found)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Jurisdictions with the Most Fair Use Cases",
       x="Jurisdiction",
       y="Number of Cases",
       fill="Outcome") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






library(countrycode)
library(tidyverse)

tt <- tidytuesdayR::tt_load('2023-04-25')
london_marathon <- tt$london_marathon
winners <- tt$winners

# Add missing data from 2022 and 2023
winners <- winners |> 
  bind_rows(data.frame(
    Category = c("Men", "Men", "Women", "Women"),
    Year = c(2022, 2023, 2022, 2023),
    Athlete = c("Amos Kipruto", "Kelvin Kiptum", "Yalemzerf Yehualaw", "Sifan Hassan"),
    Nationality = c("Kenya", "Kenya", "Ethiopia", "Netherlands"),
    Time = as.difftime(c("02:04:39", "02:01:25", "02:17:26", "02:18:33"), 
                       format="%H:%M:%S", units="secs")
  ))

# Wrangle data and include world records
df <- winners |> 
  filter(Category %in% c("Men", "Women")) |> 
  mutate(
    # reorder category levels
    Category = factor(Category, levels = c("Women", "Men")),
    # add flags
    Flag = countrycode(Nationality, 'country.name', 'unicode.symbol'),
    # add world records
    Record = case_when(
      Category == "Men"   & Year == 2023 ~ 2023,
      Category == "Women" & Year == 1983 ~ 1983,
      Category == "Women" & Year == 1985 ~ 1985,
      Category == "Women" & Year == 2002 ~ 2002,
      Category == "Women" & Year == 2003 ~ 2003,
      Category == "Women" & Year == 2005 ~ 2005,
      Category == "Women" & Year == 2017 ~ 2017,
      .default = NA
    )) |> 
  # only keep flags for world records
  mutate(Flag = ifelse(is.na(Record), NA, Flag))

# Graph
df |> 
  ggplot(aes(x = Year, y = Time)) +
  geom_vline(aes(xintercept = Record), color = "#FFFFFF") +
  geom_line(aes(color = Category), linewidth = 1.25) +
  geom_point(aes(x = Record), size = 4, shape = 18, color = "gold") +
  facet_wrap(~ Category, nrow = 2, ncol = 1, scales = "free") + 
  scale_color_manual(values = c("#E51664", "#E51664")) +
  labs(x = "", y = "",
       title = "Marathon World Records in London",
       subtitle = paste0(
         "World records for marathon running have been broken 7 times",
         "\nat the London Marathon; once for men and six times for women"),
       caption = "Source: London Marathon | github.com/huckjo") +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 18, color = "#FFFFFF"),
    plot.subtitle = element_text(size = 12, color = "#FFFFFF"),
    plot.caption = element_text(size = 8, color = "#FFFFFF"),
    plot.title.position = "plot",
    plot.background = element_rect(fill = "#0F36FF", color = "#0F36FF"),
    panel.background = element_rect(fill = "#0F36FF"),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 10, color = "#FFFFFF"),
    axis.text.x = element_text(size = 10, color = "#FFFFFF"),
    legend.position = "none",
    plot.margin = margin(.5, .5, .5, .5, "cm")
  )

df |> 
  ggplot(aes(x = Year, y = Time, group = Category)) +
  geom_vline(aes(xintercept = Record), color = "#FFFFFF") +
  geom_line(aes(color = Category), size = 1.25) +
  geom_text(aes(label = Flag), family="EmojiOne") +
  scale_color_manual(values = c("#E51664", "#000000")) +
  labs(x = "", y = "",
       title = "Marathon World Records in London",
       subtitle = paste0(
         "The marathon world record has been broken seven times at",
         "\nthe London Marathon; once for men and six times for women"),
       caption = "Source: London Marathon | github.com/huckjo") +
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 20, color = "#FFFFFF"),
    plot.subtitle = element_text(size = 14, color = "#FFFFFF"),
    plot.caption = element_text(size = 8, color = "#FFFFFF"),
    plot.title.position = "plot",
    plot.background = element_rect(fill = "#0F36FF", color = "#0F36FF"),
    panel.background = element_rect(fill = "#0F36FF"),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 10, color = "#FFFFFF"),
    axis.text.x = element_text(size = 10, color = "#FFFFFF"),
    legend.position = "none",
    plot.margin = margin(.5, .5, .5, .5, "cm")
  )

ggsave("london_marathon.png")
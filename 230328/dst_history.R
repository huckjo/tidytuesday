################################################################################
# TidyTuesday: March 28, 2023
# Daylight Savings Time
################################################################################

library(pacman)
p_load(cowplot, tidytuesdayR, tidyverse)

tt <- tidytuesdayR::tt_load('2023-03-28')
transitions <- tt$transitions
timezones <- tt$timezones
timezone_countries <- tt$timezone_countries
cities <- read.csv("230328/cities.csv")

################################################################################

dst <- transitions |> 
  # filter DST periods in U.S. cities
  inner_join(timezone_countries, by = "zone", multiple = "all") |> 
  filter(country_code == "US" & dst == TRUE) |> 
  
  # transform date type and exclude future
  mutate(begin = as_date(substr(begin, 1, 10)),
         end = as_date(substr(end, 1, 10))) |> 
  filter(year(end) <= 2023) |> 
  
  # list of days between each transition
  mutate(date = map2(begin, end, seq, by = "1 day")) |> 
  unnest(cols = c(date)) |> 
  
  # add cities, states, and present time zones
  left_join(cities, by = "zone") |> 
  mutate(city = paste(city, state, sep = ", "))

################################################################################

subtitle = paste0(
  "Explore the observance of Daylight Saving Time in the United States; how national",
  "\npolicy changes and regional differences have influenced the country's clocks, from",
  "\nthe Standard Time Act of 1918 to the Energy Policy Act of 2007."
  
  #"\n  • 1918: Standard Time Act",
  #"\n  • 1942: War Time Act",
  #"\n  • 1967: Uniform Time Act",
  #"\n  • 1974: Energy Crisis",
  #"\n  • 2007: Energy Policy Act"
)

palette <- data.frame(
  tz = c("Hawaii-Aleutian", "Alaska", "Pacific", "Mountain",  "Central", "Eastern"),
  color = c("#005f73", "#0a9396", "#ee9b00", "#ca6702", "#bb3e03", "#9b2226")
) |> deframe()

# create annotations
#annot <- data.frame(
#  city = rep("Anchorage, AK", 4), 
#  tz = rep("Alaska", 4),
#  date = c(as.Date("1918-03-19"), 
#           as.Date("1942-02-09"), 
#           as.Date("1967-04-01"), 
#           as.Date("2007-03-11")),
#  label = c("Standard Time Act", 
#            "War Time Act",
#            "Uniform Time Act",
#            "Energy Policy Act"),
#  stagger = c(1, 1.5, 1, 1.5)
#)

min_date <- min(dst$date)
max_date <- max(dst$date)

dst |> 
  filter(dst == TRUE) |> 
  ggplot() +
  geom_tile(aes(x = date, y = reorder(city, desc(state)), fill = tz),
            width = 1, height = 0.9) +
  
  # add annotations
  geom_vline(xintercept = as.numeric(as.Date(c(
    "1918-03-19", # Standard Time Act
    "1942-02-09", # War Time Act
    "1967-04-01", # Uniform Time Act
    "1974-01-06", # Energy Crisis
    "2007-03-11"  # Energy Policy Act
    ))), size = 0.5, linetype = "dashed", color = "#000000") +
  #geom_text(data = annot, aes(x = date, y = city, label = label)) +
  coord_cartesian(clip = 'off', xlim = c(min_date, max_date)) +
  scale_fill_manual(values = palette) +
  labs(x = "", y = "", 
       title = "100+ Years of Daylight Savings",
       subtitle = subtitle,
       caption = "Source: IANA tz database | github.com/huckjo") +
  
  facet_grid(tz ~ ., scales = "free", space='free') +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18, color = "#001219", ),
    plot.subtitle = element_text(size = 12, color = "#001219", margin = margin(0, 0, 5, 0)),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_text(size = 8, color = "#001219"),
    plot.background = element_rect(fill = "#e9d8a6", color = "#e9d8a6"),
    panel.background = element_rect(fill = "#e9d8a6"),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 10, color = "#001219"),
    axis.text.y = element_text(size = 10, color = "#001219"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    strip.text.y = element_text(face = "bold", size = 10, color = "#001219", angle=0, hjust=0),
    legend.position = "none",
    plot.margin = margin(.5, .5, .5, .5, "cm"),
  ) 
    
ggsave("dst_history.png")

################################################################################
# Notes

# Standard Time Act of 1918
# War Time Act of 1942
# Emergency Time Act of 1967
# Adak and Nome have a long history of moving time zones
# Arizona and Hawaii are the two states that don't observe DST
# Indiana observes Eastern Time, except for 12 counties that observe Central Time
# https://en.wikipedia.org/wiki/History_of_time_in_the_United_States
# https://en.wikipedia.org/wiki/Standard_Time_Act
# https://en.wikipedia.org/wiki/Uniform_Time_Act
# https://en.wikipedia.org/wiki/Emergency_Daylight_Saving_Time_Energy_Conservation_Act
# https://en.wikipedia.org/wiki/Time_in_Arizona
# https://en.wikipedia.org/wiki/Time_in_Indiana
# https://en.wikipedia.org/wiki/Time_in_North_Dakota
  
# https://stackoverflow.com/questions/52341385/how-to-automatically-adjust-the-width-of-each-facet-for-facet-wrap
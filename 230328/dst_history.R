################################################################################
# TidyTuesday: March 28, 2023
# Daylight Savings Time
################################################################################

library(pacman)
p_load(tidytuesdayR, tidyverse)

tt <- tidytuesdayR::tt_load('2023-03-28')
transitions <- tt$transitions
timezones <- tt$timezones
timezone_countries <- tt$timezone_countries

################################################################################

# the continential U.S. has 4 time zones (plus 5 in Alaska, Hawaii, etc.) so why
# does IANA have time zone data on 29 cities? Turns out they all have histories

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
  
  # consolidate time zone names
  mutate(tz = case_when(
    abbreviation %in% c() ~ "Eastern",
    abbreviation %in% c() ~ "Central",
    abbreviation %in% c("NWT") ~ "Mountain",
    abbreviation %in% c("PDT") ~ "Pacific",
    abbreviation %in% c() ~ "Eastern",
    abbreviation %in% c(
      "AHDT", "AKDT", "APT", "AWT", "BDT", "NPT"
      ) ~ "Alaska (GMT-8)",
    abbreviation %in% c("HDT") ~ "Hawaii-Aleutian (GMT-10)",
  ))

################################################################################

dst |> 
  filter(dst == TRUE) |> 
  ggplot(aes(x = date, y = zone)) +
  geom_tile(width = 1, height = 0.9) +
  # Congress passed the War Time Act
  geom_vline(xintercept = as.numeric(as.Date(c("1942-01-20", "1945-09-30"))),
             linetype = "dashed", color = "darkgreen") +
  annotate("text", x = as.Date("1943-12-31"), y = max(dst$zone), label = "War Time",
           color = "darkgreen", fontface = "bold", vjust = "top") +
  labs(
    x = "", y = "", 
    title = "Daylight Savings in the United States"
    ) +
  theme_minimal() 

################################################################################

# Adak and Nome have a long history of time zones
# Standard Time Act of 1918
# War Time Act of 1942
# Emergency Time Act
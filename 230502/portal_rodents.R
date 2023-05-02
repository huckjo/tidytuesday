library(pacman)
p_load(tidytuesdayR, tidyverse)

tt <- tt_load('2023-05-02')

species <- tt$species
surveys <- tt$surveys

################################################################################

df <- surveys |> left_join(species, by = "species", keep = FALSE) 

species_list <- c("DM", "OT", "PB", "PE", "PF", "PM", "PP", "RM")

populations <- df |> 
  filter(species %in% species_list) |> 
  mutate(year_month = floor_date(censusdate, "month")) |> 
  group_by(species, treatment, year_month) |> 
  summarize(count = n()) 

populations |> 
  ggplot(aes(x = year_month, y = count, color = treatment)) +
  geom_line() +
  facet_wrap(~ species) +
  labs(title = "Rodent population by species and treatment",
       x = "", y = "Count",
       color = "Treatment") +
  theme_classic()

################################################################################

species_list <- c("DM", "OT", "PB", "PE", "PF", "PM", "PP", "RM")
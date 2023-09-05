library(pacman)
p_load(geofacet, scales, tidyverse)

#tt <- tidytuesdayR::tt_load('2023-09-05')
#demog <- tt$demographics
#states <- tt$states
#wages <- tt$wages
#rm(tt)

demog <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/demographics.csv')
wages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/wages.csv')
states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/states.csv')

################################################################################
# GRAPH

states |> 
  filter(sector %in% c("Public", "Private")) |> 
  ggplot(aes(x = year, y = p_members, color = sector)) +
  geom_line() +
  facet_geo(~ state, grid = "us_state_grid2", label = "name") +
  scale_x_continuous(labels = function(x) paste0("'", substr(x, 3, 4))) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c("#DF2935", "#A57F60")) +
  labs(x = "", y = "Union Membership", color = "Sector:",
       title = "State of the Unions",
       subtitle = paste0(
         "Union membership rates in the private sector declined, while trends in public sector trends vary by state. \n",
         "Rates increased in states like California, Illinois, and Oregon, whereas Wisconsin observed the most \n",
         "dramatic decline, largely due to 2011 legislation that restricted collective bargaining."),
       caption = "Source: unionstats.com | github.com/huckjo") +
  theme(
    text = element_text(color="#ffffff"),
    plot.title = element_text(face = "bold", size = 20, color = "#00ccff"),
    plot.subtitle = element_text(size = 12, color = "#ffffff"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_text(size = 8, color = "#ffffff"),
    plot.background = element_rect(fill = "#004080", color = "#004080"),
    panel.background = element_rect(fill = "#004080"),

    panel.border = element_rect(color = "#00ccff", fill = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(size = 7, color = "#ffffff"),
    axis.text.x = element_text(size = 7, color = "#ffffff"),
    axis.text.y = element_text(size = 7, color = "#ffffff"),
    axis.ticks.y = element_blank(),
    legend.position="top",
    legend.text = element_text(size = 12, color = "#ffffff"),
    legend.title = element_text(size = 12, color = "#ffffff"),
    legend.background = element_rect(fill = "#004080"),
    legend.key  = element_rect(fill = "#004080"),
    strip.text.x = element_text(size = 7, color = "#004080"),
    strip.background = element_rect(color="#00ccff", fill="#00ccff", linetype="solid"),
    plot.margin = margin(.5, .5,.5, .5, "cm")
  )
library(tidyverse)

tt <- tidytuesdayR::tt_load('2023-09-12')
all_countries <- tt$all_countries
country_regions <- tt$country_regions
global_human_day <- tt$global_human_day
global_economic_activity <- tt$global_economic_activity
rm(tt)

# Calculate the global average for each subcategory
global_averages <- all_countries |> 
  group_by(Subcategory) |> 
  summarise(global_avg_minPerDay = 60 * sum(hoursPerDayCombined * population) / sum(population))

# Extract U.S. data for each subcategory
us_data <- all_countries |> 
  filter(country_iso3 == "USA") |> 
  select(Subcategory, hoursPerDayCombined) |> 
  mutate(us_minPerDay = hoursPerDayCombined * 60)

# Subtract global average from U.S. estimates
comparison_df <- merge(global_averages, us_data, by="Subcategory")
comparison_df$difference <- comparison_df$us_minPerDay - comparison_df$global_avg_minPerDay

# Add categories
comparison_df <- merge(comparison_df, all_countries[, c("Subcategory", "Category")], 
                       by="Subcategory") |> distinct()

# Plot graph
comparison_df |> 
  ggplot(aes(y = reorder(Subcategory, difference), x = difference, fill = Category)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(round(us_minPerDay, 0), "m"),
                  x = 0, #ifelse(difference > 0, difference, 0),
                  hjust = ifelse(difference > 0, 1.2, -0.2)), vjust=0.5, size=4) +
    labs(x = "Difference in Minutes (U.S. - Global Average)", y = "", fill = "Category: ",
         title = "A Day in the U.S. vs. the World",
         subtitle = paste0(
           "Time ticks uniformly around the world, but activities vary widely. This graph illustrates how\n",
           "Americans' time spent on activities compares to global norms. On average, Americans spend an\n",
           "extra 48 minutes on passive experiences like watching TV. With less time dedicated to growing\n",
           "food and preparing meals, the U.S. enjoys greater leisure and freedom to unwind."
         ),
         caption = "Source: Fajzel et al. (2023); Human Chronome Project | github.com/huckjo") +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 24, color = "#000000"),
      plot.subtitle = element_text(size = 12, color = "#000000"),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.caption = element_text(size = 8, color = "#000000"),
      plot.background = element_rect(fill = "#E6E6FA", color = "#E6E6FA"),
      panel.background = element_rect(fill = "#E6E6FA"),
      
      panel.border = element_rect(color = "#E6E6FA", fill = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title.x = element_text(size = 10, color = "#000000"),
      axis.text.x = element_text(size = 10, color = "#000000"),
      axis.text.y = element_text(size = 10, color = "#000000"),
      axis.ticks.y = element_blank(),
      legend.position = "none", #"bottom",
      #legend.text = element_text(size = 10, color = "#000000"),
      #legend.title = element_text(face="bold", size = 10, color = "#000000"),
      #legend.background = element_rect(fill = "#E6E6FA", color = NA),
      #legend.margin = margin(t = 0, r = -50, b = 0, l = 0, unit = "pt"),
      plot.margin = margin(.5, .5,.5, .5, "cm")
      ) 
      #guides(fill = guide_legend(nrow = 3, ncol = 3))
# TidyTuesday: April 11, 2023
# Egg Production

# Load data --------------------------------------------------------------------

library(pacman)
p_load(cowplot, tidytuesdayR, tidyverse)

# https://osf.io/z2gxn/

tt <- tidytuesdayR::tt_load('2023-04-11')
eggs <- tt$`egg-production`
cagefree <- tt$`cage-free-percentages`

# Wrangle data -----------------------------------------------------------------

# transform data for calculations
wide_eggs <- eggs |> 
  filter(prod_type == "table eggs") |> 
  mutate(prod_process = case_when(
    prod_process == "cage-free (non-organic)" ~ "nonorganic",
    prod_process == "cage-free (organic)" ~ "organic",
    prod_process == "all" ~ "all"
  )) |> 
  pivot_wider(
    id_cols = observed_month,
    names_from = prod_process,
    values_from = c(n_eggs, n_hens)
  ) 

# total eggs
eggs_all <- wide_eggs |> 
  select(observed_month, n_hens_all, n_eggs_all)

# cage-free eggs
#eggs_cagefree <- wide_eggs |> 
#  mutate(
#    n_eggs = n_eggs_organic + n_eggs_nonorganic,
#    n_hens = n_hens_organic + n_hens_nonorganic,
#    prod_type = "table eggs",
#    prod_process = "cage-free"
#  ) |> 
#  select(observed_month, prod_type, prod_process, n_hens, n_eggs)
  
# subtract conventional eggs from total
eggs_caged <- wide_eggs |> 
  mutate(
    n_eggs = n_eggs_all - (n_eggs_organic + n_eggs_nonorganic),
    n_hens = n_hens_all - (n_hens_organic + n_hens_nonorganic),
    prod_process = "caged"
  ) |> 
  select(observed_month, prod_process, n_hens, n_eggs)

# percent caged and cage-free
table_eggs <- eggs |> 
  filter(prod_type == "table eggs" & prod_process != "all") |> 
  select(observed_month, prod_process, n_hens, n_eggs) |> 
  bind_rows(eggs_caged) |> 
  left_join(eggs_all, by = "observed_month") |> 
  mutate(percent_hens = n_hens / n_hens_all * 100,
         percent_eggs = n_eggs / n_eggs_all * 100) |> 
  filter(!is.na(percent_eggs))

# Draw graphs ------------------------------------------------------------------

vol <- table_eggs |> 
  ggplot(aes(x = observed_month, y = n_eggs/1000000000, fill = prod_process)) +
  geom_area() +
  scale_fill_manual(values = c("#FB0000", "#EFAAA9", "#ECC301")) +
  labs(x = "", y = "Eggs (billions)",
       caption = " ") +
  theme_classic() +
  theme(
    plot.caption = element_text(size = 8),
    plot.background = element_rect(fill = "#fffaf0", color = "#fffaf0"),
    panel.background = element_rect(fill = "#fffaf0"),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 8),
    legend.position = "none",
    plot.margin = margin(0, 0, .5, .5, "cm")
  )

mix <- table_eggs |> 
  ggplot(aes(x = observed_month, y = percent_eggs, fill = prod_process)) +
  geom_area() +
  scale_fill_manual(values = c("#FB0000", "#EFAAA9", "#ECC301")) +
  labs(x = "", y = "% Eggs",
       caption = "Source: Humane League | github.com/huckjo") +
  theme_classic() +
  theme(
    plot.caption = element_text(size = 8),
    plot.background = element_rect(fill = "#fffaf0", color = "#fffaf0"),
    panel.background = element_rect(fill = "#fffaf0"),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 8),
    legend.position = "none",
    plot.margin = margin(0, .5, .5, 0, "cm")
  )

annual_eggs <- table_eggs |> 
  mutate(year = format(observed_month, "%Y")) |> 
  group_by(year, prod_process) |>  
  summarise(eggs = sum(n_eggs)) |>  
  arrange(prod_process, year) 

yoy <- annual_eggs |> 
  group_by(prod_process) |> 
  mutate(lag = lag(eggs, 1),
         growth = (eggs - lag(eggs, 1)) / lag(eggs, 1)) |> 
  filter(year >= 2017) |> 
  ggplot(aes(x = year, y = growth, fill = prod_process)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("#FB0000", "#EFAAA9", "#ECC301")) +
  labs(x="", y = "Growth (% YoY)",
       title = "Cracking the Egg Industry",
       subtitle = "The proportion of cage-free eggs grew to 24% of total U.S. egg \nproduction after California implemented new confinement standards" ) + 
  theme_classic() +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12),
    plot.title.position = "plot",
    #plot.caption.position = "plot",
    plot.background = element_rect(fill = "#fffaf0", color = "#fffaf0"),
    panel.background = element_rect(fill = "#fffaf0"),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 8),
    legend.text = element_text(face = "bold", size = 10),
    legend.title = element_blank(),
    legend.background = element_rect(fill = "#fffaf0"),
    legend.position = "top",
    plot.margin = margin(.5, .5, 0, .5, "cm")
  )

# Combine graphs ---------------------------------------------------------------

row <- plot_grid(vol, mix, nrow = 1, align = "v")
plot_grid(yoy, row, ncol = 1, align = "h")

ggsave("eggs.png", dpi=320)
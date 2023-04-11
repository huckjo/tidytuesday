# TidyTuesday: April 11, 2023
# Egg Production

# Load data --------------------------------------------------------------------

library(pacman)
p_load(tidytuesdayR, tidyverse)

# https://osf.io/z2gxn/

tt <- tidytuesdayR::tt_load('2023-04-11')
eggs <- tt$`egg-production`
cagefree <- tt$`cage-free-percentages`

# Explore data -----------------------------------------------------------------

# plot the eggs data
ggplot(eggs, aes(x = observed_month, y = n_eggs, 
                 color = prod_type, linetype = prod_process)) +
  geom_line(size = 1) +
  labs(x = "Month", y = "Egg Production") +
  facet_wrap(~ prod_process) +
  theme_classic()

# calculate the ratio of eggs to hens
eggs$ratio <- eggs$n_eggs / eggs$n_hens

# plot the eggs ratios
ggplot(eggs, aes(x = observed_month, y = ratio, 
                 color = prod_type, linetype = prod_process)) +
  geom_line(size = 1) +
  labs(x = "Month", y = "Egg Production") +
  facet_wrap(~ prod_process) +
  theme_classic()

# plot cage free data
cagefree |> 
  filter(!is.na(percent_eggs)) |> 
  ggplot() +
  geom_line(aes(x = observed_month, y = percent_eggs), color="red") +
  geom_line(aes(x = observed_month, y = percent_hens), color="blue") +
  theme_classic()

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
  mutate(prod_type = "table eggs") |> 
  select(observed_month, prod_type, n_hens_all, n_eggs_all)

# cage-free eggs
eggs_cagefree <- wide_eggs |> 
  mutate(
    n_eggs = n_eggs_organic + n_eggs_nonorganic,
    n_hens = n_hens_organic + n_hens_nonorganic,
    prod_type = "table eggs",
    prod_process = "cage-free"
  ) |> 
  select(observed_month, prod_type, prod_process, n_hens, n_eggs)
  
# caged eggs
eggs_caged <- wide_eggs |> 
  mutate(
    n_eggs = n_eggs_all - (n_eggs_organic + n_eggs_nonorganic),
    n_hens = n_hens_all - (n_hens_organic + n_hens_nonorganic),
    prod_type = "table eggs",
    prod_process = "caged"
  ) |> 
  select(observed_month, prod_type, prod_process, n_hens, n_eggs)

# percent caged and cage-free
table_eggs <- eggs_caged |> 
  bind_rows(eggs_cagefree) |> 
  left_join(eggs_all, by = c("observed_month", "prod_type")) |> 
  mutate(percent_hens = n_hens / n_hens_all * 100,
         percent_eggs = n_eggs / n_eggs_all * 100) |> 
  filter(!is.na(percent_eggs))

# Draw graph -------------------------------------------------------------------

table_eggs |> 
  ggplot(aes(x = observed_month, y = percent_hens, color = prod_process)) +
  geom_line(size = 1) +
  labs(x = "Month", y = "Percent of Egg Production",
       title = "Growing Percent of Eggs Are Cage-Free") +
  theme_classic()

# percent organic
eggs |> 
  filter(prod_process != "all") |> 
  mutate(prod_process = case_when(
    prod_process == "cage-free (non-organic)" ~ "non-organic",
    prod_process == "cage-free (organic)" ~ "organic"
  )) |> 
  left_join(
    eggs_cagefree |> 
      select(-prod_process) |> 
      rename(n_hens_all = n_hens, n_eggs_all = n_eggs),
    by = c("observed_month", "prod_type")
  ) |> 
  mutate(percent_hens = n_hens / n_hens_all * 100,
         percent_eggs = n_eggs / n_eggs_all * 100) |> 
  ggplot(aes(x = observed_month, y = percent_hens, color = prod_process)) +
  geom_line(size = 1) +
  labs(x = "Month", y = "Percent of Egg Production",
       title = "Growing Percent of Eggs Are Cage-Free") +
  theme_classic()



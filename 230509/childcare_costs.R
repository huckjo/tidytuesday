library(pacman)
p_load(tidyverse)

tt <- tidytuesdayR::tt_load('2023-05-09')
counties <- tt$counties
childcare_costs <- tt$childcare_costs 

rm(tt)

################################################################################

add_quartiles <- function(df, group, vars) {
  
  # Return data frame with categorical versions of each variable 
  
  for (var in vars) {
    # Create new variable name for the categorical quartile version
    new_var <- paste0(var, "_quartile")
    
    # Calculate quartiles for the variable within each group and add it to the data frame
    df <- df %>%
      mutate(!!new_var := cut(
        get(var),
        breaks = quantile(get(var), probs = 0:4/4, na.rm = TRUE, type = 7),
        labels = c("Q1", "Q2", "Q3", "Q4"),
        include.lowest = TRUE
        )
      ) 
  }
  
  return(df)
}

v <- c("mcsa", "mfccsa", "unr_16", "funr_16", "munr_16", "pr_f",
       "mhi_2018", "me_2018", "total_pop", "households",
       "h_under6_both_work", "h_under6_f_work", "h_under6_m_work")

df <- childcare_costs |> 
  mutate(
    county_fips_code = as.character(county_fips_code),
    year = as.factor(study_year)
  ) |> 
  group_by(year) |> 
  add_quartiles(vars = v)

######################################################################################

ggplot(df, aes(y=mcsa, x=unr_16)) + 
  geom_point(stat="identity", 
             position="jitter",
             alpha=0.5, size=3) + 
  geom_smooth(aes(color = year),
              stat="smooth", 
              position="identity", 
              method="lm", 
              se=TRUE, n=80, level=0.95, span=0.75) +
  facet_wrap(~ pr_f_quartile) +
  theme_classic() + 
  theme(text=element_text(family="sans", color="#000000", size=15)) +
  scale_size(range=c(1, 3)) 


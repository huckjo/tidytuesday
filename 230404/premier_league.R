################################################################################
# TidyTuesday: April 4, 2023
# Premier League
################################################################################

library(pacman)
p_load(ggbump, teamcolors, tidytuesdayR, tidyverse)

tt <- tt_load('2023-04-04')
tt <- tt_load(2023, week = 14)
soccer <- tt$`soccer21-22`
rm(tt)

################################################################################

df <- soccer |> 
  mutate(Date = strptime(Date, format = "%d/%m/%Y"),
         Match = row_number()) |>
  select(Date, Match, HomeTeam, AwayTeam, FTHG, FTAG, FTR) |> 
  pivot_longer(cols = ends_with("Team"),
               names_to = "Location", 
               values_to = "Club") |> 
  mutate(Goals = if_else(Location == "HomeTeam", FTHG, FTAG),
         Location = ifelse(Location == "HomeTeam", "H", "A"))

# use dates to assign matches to round 
df <- df |> 
  arrange(Date) |> 
  group_by(Club) |> 
  mutate(Round = row_number()) |> 
  ungroup()

# When two or more clubs have the same number of points after a round of 
# matches, the English Premier League applies a set of tie-breaking criteria 
# until the tie is broken: largest goal difference, most goals scored, most 
# points in head-to-head matches, most away goals in head-to-head matches.

df <- df |> 
  mutate(
    # assign 3 points for win; 1 for draw; 0 for loss
    Points = case_when(
      FTR == Location ~ 3,
      FTR == "D" ~ 1,
      .default = 0
    ),
    # calculate goal difference
    GoalDiff = if_else(Location == "H", FTHG - FTAG, FTAG - FTHG)
  ) |> 
  # add up goals, points, and cumulative goal difference
  group_by(Club) |> 
  arrange(Date) |> 
  mutate(
    CumulativeGoals = cumsum(Goals),
    CumulativePoints = cumsum(Points),
    CumulativeGoalDiff = cumsum(GoalDiff)
  ) |> 
  ungroup()

# calculate head-to-head points and goal difference
# head_to_head <- df |> 
#  group_by(Round, Club, Location) |>
#  summarize(H2H_Points = sum(Points),
#            H2H_GoalDiff = sum(GoalDiff)) 

# rank clubs based on the criteria
ranking <- df |> 
  #left_join(df_head_to_head, by = c("Week", "Team", "Location")) %>%
  group_by(Round) |> 
  arrange(
    desc(CumulativePoints),
    desc(CumulativeGoalDiff),
    desc(CumulativeGoals)
    #desc(HeadPoints),
    #desc(HeadGoalDifference)
  ) |> 
  mutate(Rank = row_number()) |> 
  ungroup() |> 
  select(Round, Club, Rank)

################################################################################

# assign color palette
epl_palette <- league_pal("epl") |> 
  enframe(name = "Club", value = "Color") |> 
  # rename clubs to match data
  mutate(Club = case_when(
    Club == "Brighton & Hove Albion" ~ "Brighton",
    Club == "Leicester City" ~ "Leicester",
    Club == "Manchester City" ~ "Man City",
    Club == "Manchester United" ~ "Man United",
    Club == "Newcastle United" ~ "Newcastle",
    Club == "Tottenham Hotspur" ~ "Tottenham",
    Club == "West Ham United" ~ "West Ham",
    .default = Club
  )) |> 
  # manually add clubs with missing palette
  bind_rows(
    data.frame(
      Club = c("Aston Villa", "Brentford", "Leeds", "Norwich", "Wolves"),
      Color = c("#A5C6EA", "#E40C0B", "#FFFFFF", "#2D8D3D", "#FABA09")
    )
  ) |> 
  # exclude clubs missing from 2021 season
  filter(!Club %in% c(
    "AFC Bournemouth",
    "Huddersfield Town",
    "Stoke City",
    "Swansea City",
    "West Bromwich Albion"
  )) |> 
  deframe()

# rank diagram
ranking |> 
  ggplot(aes(x = Round, y = Rank, group = Club, color = Club)) +
  geom_bump(smooth = 10, size = 1.5, alpha = .8, lineend = "round") +
  geom_text(
    data = ranking |> 
      filter(Round == 38), aes(label = Club), size = 3.5, hjust = -0.1
    ) +
  #scale_color_manual(values=epl_palette) +
  scale_x_continuous(limits = c(0, 52)) +
  scale_y_reverse(breaks = 1:20) +
  labs(x = NULL, y = NULL, fill = NULL, color = NULL,
       title = "Premier League Rankings",
       subtitle = "Manchester City quickly climbed the ranks while Liverpool and \nChelsea maintained top positions during the 2021 season",
       caption = "Source: English Premier League | github.com/huckjo") +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 18, color = "#ECBBF2"),
    plot.subtitle = element_text(size = 12, color = "#ECBBF2"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_text(size = 8, color = "#ECBBF2"),
    plot.background = element_rect(fill = "#360C3B"),
    panel.background = element_rect(fill = "#360C3B"),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 10, color = "#ECBBF2"),
    axis.ticks.y =element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x =element_blank(),
    legend.text = element_text(size = 12, color = "#ECBBF2"),
    legend.title = element_text(size = 12, color = "#ECBBF2"),
    legend.position = "none",
    plot.margin = margin(.75, 1,.75, .75, "cm"),
    aspect.ratio = 0.75
  )

ggsave("premier_league.png")
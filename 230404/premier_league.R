################################################################################
# TidyTuesday: April 4, 2023
# Premier League
################################################################################

library(pacman)
p_load(ggbump, ggnewscale, teamcolors, tidytuesdayR, tidyverse)

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
  group_by(Club) |> 
  arrange(Date) |> 
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

# rank clubs based on criteria 1-3
initial_ranks <- df |> 
  group_by(Round) |> 
  mutate(
    Rank1 = min_rank(CumulativePoints),
    Rank2 = min_rank(CumulativeGoalDiff),
    Rank3 = min_rank(CumulativeGoals)
  ) |> 
  # indicate whether club is tied each round
  arrange(Round, desc(Rank1), desc(Rank2), desc(Rank3)) |> 
  ungroup()

# create dataframe with match pairs
matches <- df |> select(Date, Match, Club)
matches <- matches |> 
  inner_join(matches, by = c("Date", "Match"),
             relationship = "many-to-many") |> 
  filter(Club.x != Club.y) |> 
  rename(Opponent = Club.y) |> 
  select(Date, Match, Club = Club.x, Opponent) 

# compute head-to-head criteria
ranking <- initial_ranks |> 
  inner_join(matches, by = c("Date", "Match", "Club")) %>%
  group_by(Club, Opponent) |> 
  arrange(Date) |> 
  mutate(
    HeadPoints = cumsum(Points),
    AwayGoals = cumsum(FTAG)
  ) |> 
  ungroup() |> 
  # final rankings
  group_by(Round) |> 
  arrange(Round) |> 
  mutate(
    Rank4 = min_rank(HeadPoints),
    Rank5 = min_rank(AwayGoals)
  ) |> 
  arrange(
    Round, desc(Rank1), desc(Rank2), desc(Rank3), desc(Rank4), desc(Rank5)
    ) |> 
  mutate(Rank6 = row_number())

################################################################################

premier_palette <- c("#38003c", "#00ff85", "#e90052", "#04f5ff")

# assign color palette
club_palette <- league_pal("epl") |> 
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
    .default = Club)
  ) |> 
  mutate(Color = case_when(
    # manually change dark to lighter color
    Club == "Newcastle" ~ "#37B6F1",
    Club == "Tottenham" ~ "#FFFFFF",
    .default = Color
  )) |> 
  # manually add clubs with missing palette
  bind_rows(
    data.frame(
      Club = c("Aston Villa", "Brentford", "Leeds", "Norwich", "Wolves"),
      Color = c("#A5C6EA", "#E40C0B", "#FFFFFF", "#2D8D3D", "#FABA09")
    )
  ) |> 
  deframe()

# assign palettes for text color and fill
club_text <- club_palette |> 
  enframe(name = "Club", value = "Color") |> 
  mutate(Color = case_when(
    # manually change darker to lighter color
    Club %in% c("Aston Villa", "Burnley", "Leeds", 
                "Man City", "Tottenham", "Watford") ~ "#000000",
    .default = "#FFFFFF"
  )) |> 
  deframe()

# rank diagram
ranking |> ggplot() +
  geom_bump(aes(x = Round, y = Rank6, group = Club, color = Club),
            size = 1.5, alpha = 0.9, lineend = "round") +
  scale_color_manual(values=club_palette) +
  
  # create a new scale for labels
  new_scale_color() +
  geom_label(
    data = ranking |> filter(Round == 38), 
    aes(label = Club, x = Round, y = Rank6, 
        color = Club, fill = Club, fontface = "bold"),
    size = 3.5, hjust = -0.1, label.size = NA
    ) +
  scale_color_manual(values = club_text) +
  scale_fill_manual(values = club_palette) +
  scale_x_continuous(limits = c(0, 50)) +
  scale_y_reverse(breaks = 1:20) +
  labs(x = NULL, y = NULL, fill = NULL, color = NULL,
       title = "Premier League Rankings",
       subtitle = "Manchester City quickly climbed the ranks while Liverpool and \nChelsea maintained top positions during the 2021-22 season",
       caption = "Source: English Premier League | github.com/huckjo") +
  theme(
    plot.title = element_text(face = "bold", size = 20, color = "#FFFFFF"),
    plot.subtitle = element_text(size = 14, color = "#FFFFFF"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_text(size = 8, color = "#FFFFFF"),
    plot.background = element_rect(fill = "#1f1f1f", color = "#1f1f1f"),
    panel.background = element_rect(fill = "#1f1f1f"),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 10, color = "#FFFFFF"),
    axis.ticks.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.text = element_text(size = 12, color = "#FFFFFF"),
    legend.title = element_text(size = 12, color = "#FFFFFF"),
    legend.position = "none",
    plot.margin = margin(.5, .5,.5, .5, "cm"),
    aspect.ratio = 0.75
  )

ggsave("premier_league.png", dpi=320)
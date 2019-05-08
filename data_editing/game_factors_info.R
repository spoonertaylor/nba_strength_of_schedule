# PURPOSE: Add road trip and other game time factors to games dataset.
library(tidyverse)

# * DATA ----
games = read.csv('Documents/nba_strength_of_schedule/data/games_with_rpm_long.csv',
                 stringsAsFactors = FALSE)

teams = read.csv('Documents/nba_strength_of_schedule/data/nba_bball_team_id.csv',
                 stringsAsFactors = FALSE)

# Join the home team information onto games data
games = games %>% mutate(
  home_team = ifelse(is_home, team_name, opponent_name)) %>%
  left_join(teams %>% select(-bbref_team_id, -espn_bbref_team_id), by = c("home_team" = "team_name"))

# * GAME FACTORS ----
# ** GAME NUMBER, WINS, LOSSES ----
games = games %>% arrange(game_code) %>%
  group_by(season, team_name) %>% 
  mutate(
    game_number = row_number(),
    wins = cumsum(ifelse(team_outcome == 'W', 1, 0)),
    losses = cumsum(ifelse(team_outcome == 'L', 1, 0))
  )
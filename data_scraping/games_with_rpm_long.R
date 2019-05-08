# PURPOSE: Take the games_with_rpm.csv file and make it long format
## So each game will have two rows that belong to it.
library(tidyverse)

games = read.csv('Documents/nba_strength_of_schedule/data/games_with_rpm.csv',
                 stringsAsFactors = FALSE)
games = games %>% select(-contains('X', ignore.case = FALSE))

# Take all the home teams to be the 'team' and away team's to be the 'opponent'
home = games %>% mutate(
  team_name = home_team,
  team_points = home_points,
  team_rpm = home_rpm,
  opponent_name = away_team,
  opponent_points = away_points,
  opponent_rpm = away_rpm,
  team_outcome = ifelse(home_points > away_points, 'W', 'L'),
  is_home = TRUE
) %>% select(
  game_date, game_time_start, game_code, season,
  team_name, team_points, team_rpm, opponent_name, opponent_points, opponent_rpm,
  team_outcome, is_home
)
# Now do the same for away team as the team
away = games %>% mutate(
  team_name = away_team,
  team_points = away_points,
  team_rpm = away_rpm,
  opponent_name = home_team,
  opponent_points = home_points,
  opponent_rpm = home_rpm,
  team_outcome = ifelse(away_points > home_points, 'W', 'L'),
  is_home = FALSE
) %>% select(
  game_date, game_time_start, game_code, season,
  team_name, team_points, team_rpm, opponent_name, opponent_points, opponent_rpm,
  team_outcome, is_home
)

# Bind together and save
games_long = rbind(home, away) %>% arrange(game_code)
write.csv(games_long, file = 'Documents/nba_strength_of_schedule/data/games_with_rpm_long.csv',
          row.names = FALSE)
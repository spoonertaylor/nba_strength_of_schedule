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
# Fill in Bobcats
games = games %>% mutate(
  latitude = ifelse(home_team == 'Charlotte Bobcats', 35.227, latitude),
  longitude = ifelse(home_team == 'Charlotte Bobcats', 80.843, longitude),
  time_zone = ifelse(home_team == 'Charlotte Bobcats', 'eastern', time_zone)
)

# * GAME FACTORS ----
# ** GAME NUMBER, WINS, LOSSES ----
games = games %>% arrange(game_code) %>%
  group_by(season, team_name) %>% 
  mutate(
    game_number = row_number(),
    wins = cumsum(ifelse(team_outcome == 'W', 1, 0)),
    losses = cumsum(ifelse(team_outcome == 'L', 1, 0))
  )

# ** DAYS SINCE ----
games = games %>% arrange(game_code) %>%
  group_by(season, team_name) %>%
  mutate(last_game_date = lag(game_date, order_by = game_date)) %>%
  mutate(days_since_last_game = as.Date(game_date) - as.Date(last_game_date)) %>%
  # Fill first game of the season with -1
  mutate(days_since_last_game = ifelse(is.na(days_since_last_game), -1, days_since_last_game))

# Opponent
games = games %>% arrange(game_code) %>%
  group_by(season, opponent_name) %>%
  mutate(opp_last_game_date = lag(game_date, order_by = game_date)) %>%
  mutate(opp_days_since_last_game = as.Date(game_date) - as.Date(opp_last_game_date))
  

# ** HOME STANDS/AWAY TRIPS ----
games = games %>% arrange(game_code) %>%
  group_by(season, team_name) %>%
  mutate(home_stand = ave(is_home, cumsum(!is_home), FUN = cumsum),
         road_trip = ave(!is_home, cumsum(is_home), FUN = cumsum))

# ** ROAD TRIP LENGTH ----
games = games %>% arrange(game_code) %>%
  group_by(season, team_name) %>%
  mutate(days_since_last_home_game = ave(ifelse(is_home, 0, days_since_last_game), cumsum(is_home), FUN = cumsum))

# Opponent
games = games %>% arrange(game_code) %>%
  group_by(season, opponent_name) %>%
  mutate(opp_days_since_last_home_game = ave(ifelse(!is_home, 0, opp_days_since_last_game), cumsum(!is_home), FUN = cumsum))

# ** MILES ----
games = games %>% arrange(game_code) %>%
  group_by(season, team_name) %>%
  mutate(last_lon = lag(longitude, order_by = game_code),
         last_lat = lag(latitude, order_by = game_code)) %>%
  rowwise() %>%
  mutate(miles_from_last_game = geosphere::distHaversine(p1 = c(longitude, latitude),
                                                         p2 = c(last_lon, last_lat))
  ) %>% ungroup() %>%
  # Convert to miles
  mutate(miles_from_last_game = miles_from_last_game / 1609.344) %>%
  select(-last_lat, -last_lon)
# Opponent
games = games %>% arrange(game_code) %>%
  group_by(season, opponent_name) %>%
  mutate(last_lon = lag(longitude, order_by = game_code),
         last_lat = lag(latitude, order_by = game_code)) %>%
  rowwise() %>%
  mutate(opp_miles_from_last_game = geosphere::distHaversine(p1 = c(longitude, latitude),
                                                             p2 = c(last_lon, last_lat))
  ) %>% ungroup() %>%
  # Convert to miles
  mutate(opp_miles_from_last_game = opp_miles_from_last_game / 1609.344) %>%
  select(-last_lat, -last_lon)

# Miles on current road trip
games = games %>% arrange(game_code) %>%
  group_by(season, team_name) %>%
  mutate(miles_since_last_home_game = ave(ifelse(is_home, 0, miles_from_last_game), cumsum(is_home), FUN = cumsum)) %>%
  mutate(miles_since_last_home_game = ifelse(lag(miles_since_last_home_game, order_by = game_code) > 0 & miles_since_last_home_game == 0, 
                                             miles_from_last_game + lag(miles_since_last_home_game, order_by = game_code),
                                             miles_since_last_home_game))

# ** TIME ZONES ----
games = games %>% arrange(game_code) %>%
  group_by(season, team_name) %>%
  mutate(changed_time_zone = ifelse(time_zone == lag(time_zone, order_by = game_code),
                                    FALSE,
                                    TRUE),
         time = case_when(
           time_zone == 'eastern' ~ 3,
           time_zone == 'central' ~ 2,
           time_zone == 'mountain' ~ 1,
           time_zone == 'pacific' ~ 0
         )) %>%
  mutate(time_change = time - lag(time, order_by = game_code)) %>%
  select(-time)

# ** ROLLING RPM ----
games = games %>% arrange(game_code) %>%
  group_by(season, team_name) %>%
  mutate(rolling_team_rpm = lag(team_rpm, order_by = game_code)) %>%
  group_by(season, team_name, isna = is.na(rolling_team_rpm)) %>%
  mutate(rolling_team_rpm = ifelse(isna, NA,
                                    cummean(rolling_team_rpm * (game_number-1)) / cummean((game_number-1)))) %>% 
  ungroup() %>% select(-isna) %>%
  group_by(season, opponent_name) %>%
  mutate(rolling_opponent_rpm = lag(opponent_rpm, order_by = game_code)) %>%
  group_by(season, opponent_name, isna = is.na(rolling_opponent_rpm)) %>%
  mutate(rolling_opponent_rpm = ifelse(isna, NA,
                                       cummean(rolling_opponent_rpm * (game_number-1)) / cummean((game_number-1)))) %>%
  ungroup() %>% select(-isna)

# ** SAVE ----
write.csv(games, file = 'Documents/nba_strength_of_schedule/data/games_with_factors.csv',
          row.names = FALSE)
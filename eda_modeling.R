# PURPOSE: Model the relationship between winning and losing 
## and different travel factors
library(tidyverse)

games = read.csv('Documents/nba_strength_of_schedule/data/games_with_factors.csv',
                 stringsAsFactors = FALSE)
# * EDA ----
# ** RPM Numbers ----
team_success = games %>% group_by(season, team_name) %>% summarise(
  team_rpm = mean(team_rpm),
  opponent_rpm = mean(opponent_rpm),
  wins = max(wins),
  losses = max(losses)
)

cor(team_success$team_rpm, team_success$wins)
cor(team_success$opponent_rpm, team_success$wins)

ggplot(team_success %>% filter(season == 2019), aes(x = wins, y = opponent_rpm)) +
  geom_text(aes(label = team_name))

# ** Do the best players win? ----
games_short = read.csv('Documents/nba_strength_of_schedule/data/games_with_rpm.csv',
                       stringsAsFactors = FALSE)
with(games_short,
     mean(
      (home_points > away_points & home_rpm > away_rpm) |
      (away_points > home_points & away_rpm > home_rpm)
    )
)

# ** Home court ----
home_court = games %>% group_by(team_name) %>%
  filter(is_home) %>%
  summarise(home_court_perc = mean(team_outcome == 'W'))

# ** Most miles traveled ----
miles = games %>% group_by(season, team_name) %>% summarise(miles_traveled = sum(miles_from_last_game, na.rm = TRUE)) %>%
  arrange(desc(miles_traveled))

# ** Longest road trip length ----
days_rest = games %>% group_by(season, team_name) %>% summarise(avg_days_rest = mean(days_since_last_game, na.rm = TRUE)) %>%
  arrange(avg_days_rest)
ggplot(days_rest, aes(x = season, y = avg_days_rest, group = season)) + geom_boxplot()

road_trip_lengths = games %>% group_by(season, team_name) %>%
  filter((lead(road_trip, order_by = game_code) == 0 | is.na(lead(road_trip, order_by = game_code))) &
           road_trip > 0)

longest_road_trips = road_trip_lengths %>% group_by(season, team_name) %>%
  summarise(longest_road_trip_days = max(days_since_last_home_game),
            longest_road_trip_games = max(road_trip))

# ** Days rest ----
ggplot(games %>% filter(days_since_last_game > 0), 
       aes(x = days_since_last_game, fill = as.factor(season))) + 
  geom_bar(position = 'dodge', color = 'grey3') +
  scale_x_continuous(name = "Days Since Last Game", breaks = 1:max(games$days_since_last_game), labels = 1:max(games$days_since_last_game)) +
#  ggthemes::theme_fivethirtyeight() +
  ggthemes::scale_fill_calc() +
  theme(legend.position = 'top') +
  guides(fill = guide_legend(nrow = 1)) +
  ylab("") +
  labs(fill = "Season")

# * MODEL ----
games = games %>% mutate(team_outcome_binary = ifelse(team_outcome == 'W', 1, 0))

# ** Win Game ----
mod = glm(team_outcome_binary ~ opponent_rpm + 
            is_home + days_since_last_game + time_change + miles_from_last_game +
            opp_days_since_last_game + opp_days_since_last_home_game + opp_miles_from_last_game +
            miles_from_last_game, data = games)
summary(mod)

games = games %>% mutate(
  predicted_game_outcome = ifelse(predict(mod, games, type = "response") >= 0.5, 1, 0),
  predicted_game_prob = predict(mod, games, type = "response")
)

season_2019 = games %>% filter(season == 2019) %>%
  group_by(team_name) %>% 
  summarise(actual_wins = max(wins),
            predicted_prob = mean(predicted_game_prob, na.rm = TRUE),
            predicted_wins = sum(predicted_game_outcome, na.rm = TRUE),
            avg_team_rpm = mean(team_rpm),
            avg_opponent_rpm = mean(opponent_rpm)) %>%
  arrange(predicted_prob) %>%
  mutate(strength_of_schedule = rank(predicted_wins, ties.method = "first")) %>%
  select(team_name,strength_of_schedule)

# ** Points ----
mod_points = lm(team_points - opponent_points ~ as.factor(season) +
                  team_rpm + opponent_rpm +
                  is_home + days_since_last_game + time_change, data = games %>% filter(!is.na(last_game_date)))
summary(mod_points)
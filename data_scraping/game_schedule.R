# PURPOSE: Get schedule of games for every regular season game since a certain date

# * Load library ----
library(rvest)
`%>%` = dplyr::`%>%`

# * Global variables ----
months = c('october', 'november', 'december', 'january',
           'february', 'march', 'april')
years = 2008:2019

# * Function ----
#' get_games_in_month
#' @name get_games_in_month
#' @title Scrape NBA game schedules from basketball-reference
#' @description Takes in a NBA season-year and a month during the season to scrape all
#' the games during that month
#' 
#' @param year NBA season, for example 2018-19 season is 2019
#' @param month Full length month name
#' @return Data frame with one row for each game with
#' Columns: 
#'  season, game_date, game_time_start, home_team, away_team, home_points, 
#'  away_points, game_code
get_games_in_month = function(year, month) {
  month = tolower(month)
  # Create url
  url = paste0('https://www.basketball-reference.com/leagues/NBA_', year, '_games-', month, '.html')
  # Read in page
  page = tryCatch({
    xml2::read_html(url)
  },
    error = function(e) {
      warning(paste0('Unable to read for ', month, ' ', year))
      NULL
    }
  )
  # If there aren't any games, return an empty data frame
  if (!is.null(page)) {
    games = tryCatch({
      page %>% rvest::html_node('body') %>%
        rvest::html_nodes(xpath = '//*[@id="schedule"]') %>%
        rvest::html_table()
    },
    error = function(e) {
      warning(paste0('Unable to read for ', month, ' ', year))
      return(data.frame())
    }
    )    
  }
  else {
    return(data.frame())
  }
  
  games = games[[1]]
  if (nrow(games) == 0) {
    return(data.frame())
  }  
  games = games[,1:6]
  colnames(games) = c('game_date', 'game_time_start', 'away_team', 'away_points',
                      'home_team', 'home_points')
  
  # Get rid of games that have not happened yet and any playoffs game
  if ('Playoffs' %in% games$home_team) {
    playoff_row = which(games$home_team == 'Playoffs')
    games = games[-c(playoff_row:nrow(games)),]
  }
  # Clean the games
  games = games %>% dplyr::filter(!is.na(home_points))
  games = games %>% dplyr::mutate(game_date = as.Date(game_date, '%a, %b %d, %Y'),
                                  season = year)
  games = games %>% dplyr::mutate(home_points = as.integer(home_points),
                                  away_points = as.integer(away_points))
  
  games = dplyr::left_join(games,
                           teams %>% dplyr::select(team_name, bbref_team_id),
                           by = c("home_team" = "team_name"))
  
  # Add the bbref game code
  games = games %>% dplyr::mutate(game_date2 = stringr::str_replace_all(game_date, "-", "")) %>%
            dplyr::mutate(game_code = 
                            ifelse(home_team == 'Charlotte Bobcats',
                                 paste0(game_date2, '0CHA'),
                                 paste0(game_date2, '0', bbref_team_id)))
  
  games = games %>% dplyr::select(-game_date2, -bbref_team_id)
  # # For each game, create a row for the home team and the away team
  # home_teams = games %>% dplyr::mutate(location = 'home',
  #                                      result = ifelse(home_points > away_points, 'win', 'loss')) %>%
  #   dplyr::select(season, game_date, game_time_start,
  #                 team_name = home_team,
  #                 team_points = home_points,
  #                 opponent_name = away_team,
  #                 opponent_points = away_points,
  #                 result,
  #                 location)
  # 
  # away_teams = games %>% dplyr::mutate(location = 'away',
  #                                      result = ifelse(home_points < away_points, 'win', 'loss')) %>%
  #   dplyr::select(season, game_date, game_time_start,
  #                 team_name = away_team,
  #                 team_points = away_points,
  #                 opponent_name = home_team,
  #                 opponent_points = home_points,
  #                 result,
  #                 location)
  # 
  # games = rbind(home_teams, away_teams)
  return(games)
} 

# * Run function ----
teams = read.csv('../data/nba_bball_team_id.csv', stringsAsFactors = FALSE)
i = 1
game_list = list()
for (year in years) {
  games = lapply(months, function(x) get_games_in_month(year, x))
  
  games = dplyr::bind_rows(games)
  
  game_list[[i]] = games
  i = i + 1
}

games_all = dplyr::bind_rows(game_list)

# * Save game data ----
write.csv(games_all, file = "../data/games_all_wide.csv")

# * Add missing games ----
games = read.csv('Documents/nba_strength_of_schedule/data/games_with_rpm.csv',
                 stringsAsFactors = FALSE)
games = games %>% select(-contains("X"))
# Scrape games after 4/3/2019:
new_games = get_games_in_month(2019, 'april')
new_games = new_games %>% filter(game_date > as.Date('2019-04-03'))
new_games = new_games %>% mutate(home_rpm = NA, away_rpm = NA)
games = rbind(games, new_games)
write.csv(games, file = 'Documents/nba_strength_of_schedule/data/games',
          row.names = FALSE)

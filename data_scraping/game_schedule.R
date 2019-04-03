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
#' @return Data frame with two rows for each game with each team in the game as the 'team_name'
#' Columns: season, game_date, game_time_start, team_name, team_points, opponent_name, opponent_points,
#' result, location
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

  # For each game, create a row for the home team and the away team
  home_teams = games %>% dplyr::mutate(location = 'home',
                                       result = ifelse(home_points > away_points, 'win', 'loss')) %>%
    dplyr::select(season, game_date, game_time_start,
                  team_name = home_team,
                  team_points = home_points,
                  opponent_name = away_team,
                  opponent_points = away_points,
                  result,
                  location)

  away_teams = games %>% dplyr::mutate(location = 'away',
                                       result = ifelse(home_points < away_points, 'win', 'loss')) %>%
    dplyr::select(season, game_date, game_time_start,
                  team_name = away_team,
                  team_points = away_points,
                  opponent_name = home_team,
                  opponent_points = home_points,
                  result,
                  location)

  games = rbind(home_teams, away_teams)
  return(games)
} 

# * Run function ----
i = 1
game_list = list()
for (year in years) {
  games = lapply(months, function(x) get_games_in_month(year, x))
  
  games = dplyr::bind_rows(games)
  
  game_list[[i]] = games
  i = i + 1
}

games_all = dplyr::bind_rows(game_list)

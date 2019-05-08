# PURPOSE: Scrape box scores for each game and join rpm to minutes of box score
# * Global Functions ----
`%>%` = dplyr::`%>%`
`%!in%` = function(x,y) !x%in%y

# * Functions ----
#' @name bball_ref_boxscore
#' @title Scrape boxscore for a given team and given game
#'
#' @param team team_code for the team of interest
#' @param game_code a bbref game code
#' @return Return a box score with the team code and the season
bball_ref_boxscore = function(team, game_code) {
  url = paste0('https://www.basketball-reference.com/boxscores/', game_code, '.html')
  
  page = tryCatch({
    xml2::read_html(url)
  },
  error = function(e) {
    warning(paste0('Unable to read for ', game_code))
    NULL
  }
  )
  if (is.null(page)) {
    return(data.frame())
  }
  body = rvest::html_node(page, 'body')
  
  table = rvest::html_nodes(body, xpath = paste0('//*[@id="box_',tolower(team),'_basic"]')) %>%
    rvest::html_table()
  
  if (length(table) == 1) {
    table = table[[1]]
  }
  else {
    warning(paste0("No boxscore for game code ", game_code, " and team ", team))
  }
  
  col_names = table[1,]
  colnames(table) = col_names
  table = table[-1,]
  
  table = table %>% dplyr::filter(stringr::str_detect(MP, ":"),
                                      Starters != 'Team Totals')
  
  table = cbind(table, team = toupper(team))
  
  # Get the season so we can join RPM numbers to it
  date = paste0(substr(game_code, 1, 4), "-", substr(game_code, 5, 6),
                "-", substr(game_code, 7, 8))
  date = as.Date(date, format = "%Y-%m-%d")
  year = as.integer(substr(game_code, 1, 4))
  if (lubridate::month(date) <= 12 & lubridate::month(date) >= 10) {
    year = year + 1
  }

  table$season = year
  
  return(table)
}

#' @name get_team_strength
#' @title Join the player's minutes with rpm value and find team value
#' 
#' @param boxscore A boxscore from function bball_ref_boxscore
#' @return A weighted mean of the RPM numbers for the players that played in the game
get_team_strength = function(boxscore) {
  # Get only minutes from boxscore and convert to proportion of teams minutes
  box_minutes = boxscore %>% dplyr::select(Starters, MP, team, season) %>%
    dplyr::mutate(minutes_numeric = as.numeric(lubridate::ms(MP))) %>%
    dplyr::mutate(prop_minutes = minutes_numeric / sum(minutes_numeric)) %>%
    dplyr::select(Starters, team, season, prop_minutes) %>%
    dplyr::mutate(Starters = tolower(stringr::str_replace_all(Starters, "[.-]", "")))
  
  # Get the espn team from the bbref team
  bbref = as.character(unique(box_minutes$team)[1])
  season_year = as.integer(unique(box_minutes$season)[1])
  # For the old bobcats
  if (bbref == 'CHA') {
    espn_team = 'CHA'
  }
  else {
    espn_team = as.character(teams[teams$bbref_team_id == bbref, 'espn_bbref_team_id'])    
  }
  # Get only rpm numbers for the selected team in season
  rpm_only = rpm %>% dplyr::filter(team == espn_team,
                                   season == season_year) %>%
    dplyr::mutate(rpm = as.numeric(rpm),
                  name = tolower(stringr::str_replace_all(name, "[.-]", ""))) %>%
    dplyr::select(name, rpm)
  
  # Join boxscore with rpm numbers
  join_data = dplyr::inner_join(box_minutes, rpm_only, by = c('Starters' = 'name'))
  # If the join wasn't perfect, just get rid of the duplicated names
  if (sum(duplicated(join_data$Starters)) > 0) {
    dup_name = join_data[duplicated(join_data$Starters), 'Starters']
    join_data = join_data %>% filter(Starters %!in% dup_name)
  }
  
  # multiply rpm and prop minutes and sum
  weight = sum(join_data$prop_minutes*join_data$rpm)
  
  return(weight)
}

# * Data ----
teams = read.csv('Documents/nba_strength_of_schedule/data/nba_bball_team_id.csv', stringsAsFactors = FALSE)
rpm = read.csv('Documents/nba_strength_of_schedule/data/nba_rpm.csv', stringsAsFactors = FALSE)
games = read.csv('Documents/nba_strength_of_schedule/data/games_with_rpm.csv', stringsAsFactors = FALSE)
games = games %>% dplyr::filter(season %in% c(2014:2019))
games$home_rpm = NA
games$away_rpm = NA

# * Add weight to games data frame ----
for (i in 3603:nrow(games)) {
  print(paste0("Game ", i, " of ", nrow(games), ": ", i/nrow(games)))
#  if (all(!is.na(games[i, "home_rpm"]), !is.na(games[i, "away_rpm"]))) {
#    next
#  }

  game_code = games[i, "game_code"]
  
  # Get home team weight
  if (games[i, "home_team"] == 'Charlotte Bobcats') {
    home_boxscore = bball_ref_boxscore('CHA', game_code)
    home_team = 'CHA'
  }
  else {
    home_boxscore = bball_ref_boxscore(teams[teams$team_name == games[i, "home_team"], 'bbref_team_id'], game_code)    
    home_team = teams[teams$team_name == games[i, "home_team"], 'bbref_team_id']
  }
  # Get away team weight
  if (games[i, "away_team"] == 'Charlotte Bobcats') {
    away_boxscore = bball_ref_boxscore('CHA', game_code)
    away_team = 'CHA'
  }
  else {
    away_boxscore = bball_ref_boxscore(teams[teams$team_name == games[i, "away_team"], 'bbref_team_id'], game_code)    
    away_team = teams[teams$team_name == games[i, "away_team"], 'bbref_team_id']
  }  
  
  home_weight = get_team_strength(home_boxscore)
  games[i, "home_rpm"] = home_weight
  away_weight = get_team_strength(away_boxscore)
  games[i, "away_rpm"] = away_weight
  
}

games = games %>% select(-contains("X"))
# * Save data ----
write.csv(games, file = "Documents/nba_strength_of_schedule/data/games_with_rpm.csv",
          row.names = FALSE)
# need to update this kind of dependency stuff
load_all('/Users/jim/Documents/nba.modelR')

# research data ----
# get the gamelogs and tidy 'em up a bit, using functions from nba.modelR
gamelogs_path <- '/Users/jim/Documents/gambling_stuff/data/nba_gamelogs/'
gamelogs_df <- nba.modelR::get_data(data_path = gamelogs_path)
tidy_gamelogs_df <- nba.modelR::tidyup_data(gamelogs_df,
                                            date_fields = c('game_date'),
                                            numeric_fields = c('min', 'fgm', 'fga', 'fg_pct', 'fg3m', 'fg3a', 'fg3_pct', 'ftm', 'fta', 'ft_pct', 'oreb', 'dreb', 'reb', 'ast', 'tov', 'stl', 'blk', 'blka', 'pf', 'pfd', 'pts', 'nba_fantasy_pts', 'dd2', 'td3', 'fd', 'dk'))
# subset only the columns we'll need for research and joining with the other data
research_data <- tidy_gamelogs_df %>%
  transmute(tidyplayer = player_name,
            tidyteam = team_abbreviation,
            game_date,
            points = pts,
            assists = ast,
            rebounds = reb,
            three_pointers = fg3m)

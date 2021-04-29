# get gambling_stuff data ----
library(tidyverse)
load_all()
# get fpts and fpts
first_team_to_score <- read.csv(
  '/server/path'
)
first_player_to_score <- read.csv(
  '/server/path'
)
# get the rosters for player-team info
rosters <- read.csv('/server/path') %>%
  transmute(
    tidyplayer = normalize_names(
      PLAYER_NAME,
      key = system.file('lu', 'nba', 'player', 'lu.json', package = 'betfinder')
    ),
    tidyteam = normalize_names(
      TEAM_ABBREVIATION,
      key = system.file('lu', 'nba', 'team', 'lu.json', package = 'betfinder')
    ))
# get the latest lineups to create a schedule for matchup info
schedule <- read.csv(
  '/server/path'
) %>%
  mutate(tidyteam = normalize_names(
    TEAM_ABBREVIATION,
    key = system.file('lu', 'nba', 'team', 'lu.json', package = 'betfinder')
  )) %>%
  select(MATCHUP, tidyteam, HOME_AWAY) %>%
  distinct() %>%
  group_by(MATCHUP) %>%
  mutate(tidyteam = tidyteam,
         tidyopp = rev(tidyteam),
         home_away = if_else(grepl('home', HOME_AWAY), 'home', 'away')) %>%
  ungroup() %>%
  select(-MATCHUP, -HOME_AWAY)

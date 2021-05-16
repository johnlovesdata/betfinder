# schedule ----
today_fn <- paste0(gsub('-', '', as.character(Sys.Date())), '.csv')
tomorrow_fn <- paste0(gsub('-', '', as.character(Sys.Date() + 1)), '.csv')
schedule <- read.csv(paste0('/home/john/gambling_stuff/data/nba_schedules/', today_fn))
# wrap tomorrow's schedule in a try cuz of last games in season errors
tomorrow <- try(read.csv((paste0('/home/john/gambling_stuff/data/nba_schedules/', tomorrow_fn))))
if (!inherits(tomorrow, 'try-error')) schedule <- bind_rows(schedule, tomorrow)
schedule <- schedule %>%
  mutate(
    # combine date and gamestart into a single datetime object
    game_datetime = lubridate::ymd_hm(paste(as.Date(GAME_DATE_EST), GAME_STATUS_TEXT), tz = "EST"),
    # correct for the fact that the game times are always EST but we're running this in CST
    corrected_datetime = game_datetime + lubridate::hours(-1) + lubridate::minutes(10),
    # extract the team abbreviations and code for home and away
    teams = gsub('.*/', '', GAMECODE),
    away = substr(teams, 1, 3),
    home = substr(teams, 4, 6)
  ) %>%
  pivot_longer(
    c(home, away),
    names_to = 'home_away',
    values_to = 'tidyteam'
  ) %>%
  group_by(teams) %>%
  mutate(tidyopp = rev(tidyteam)) %>%
  ungroup() %>%
  select(-teams) %>%
  filter(corrected_datetime >= Sys.time()) %>%
  group_by(tidyteam) %>%
  filter(corrected_datetime == min(corrected_datetime)) %>%
  ungroup() %>%
  select(game_datetime, corrected_datetime, tidyteam, home_away, tidyopp)

# projections ----
projections <-
  # first player to score
  read.csv('/home/john/gambling_stuff/data/02_curated/nba_first_to_score/first_player_to_score.csv.gz') %>%
  # first team to score
  bind_rows(
    read.csv('/home/john/gambling_stuff/data/02_curated/nba_first_to_score/first_team_to_score.csv.gz') %>%
      mutate(jumper = tidyplayer,
             tidyplayer = 'team')) %>%
  ## HERE IS WHERE WE CAN CURATE THE FIELDS INCLUDED FOR ADDITIONAL CONTEXT
  select(tidyplayer, tidyteam, prop, projected_prob, jumper_injury_status) %>%
  group_by(tidyteam) %>%
  fill(jumper_injury_status, .direction = 'updown') %>%
  ungroup() %>%
  transmute(jumper = if_else(grepl('team', prop), tidyplayer, NA_character_),
            tidyplayer = if_else(grepl('team', prop), 'team', tidyplayer),
            tidyteam = tidyteam,
            prop = prop,
            projected_prob = projected_prob,
            jumper_injury_status)

# player data ----
player_data <-
  # rosters
  read.csv('/home/john/gambling_stuff/data/02_curated/nba_rosters/current.csv.gz') %>%
  mutate(tidyplayer = normalize_names(PLAYER_NAME, key = system.file('lu', 'nba', 'player', 'lu.json', package = 'betfinder')),
         tidyteam = normalize_names(TEAM_ABBREVIATION, key = system.file('lu', 'nba', 'team', 'lu.json', package = 'betfinder'))) %>%
  select(-PLAYER_NAME, -TEAM_ABBREVIATION) %>%
  # lineups
  left_join(
    read.csv('/home/john/gambling_stuff/data/02_curated/nba_lineups/rotowire.csv') %>%
      mutate(tidyteam = normalize_names(TEAM_ABBREVIATION, key = system.file('lu', 'nba', 'team', 'lu.json', package = 'betfinder')),
             tidyplayer = normalize_names(PLAYER_NAME, key = system.file('lu', 'nba', 'player', 'lu.json', package = 'betfinder'))) %>%
      select(-PLAYER_NAME, -TEAM_ABBREVIATION),
    by = c('tidyteam', 'tidyplayer')) %>%
  group_by(tidyplayer) %>%
  fill(tidyteam, .direction = 'updown') %>%
  ungroup() %>%
  group_by(tidyplayer) %>%
  arrange(desc(LINEUP_DESC)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  transmute(
    tidyplayer, tidyteam,
    injury_status = TO_PLAY_DESC,
    starter_status = LINEUP_DESC)


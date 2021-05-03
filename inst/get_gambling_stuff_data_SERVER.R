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
  select(tidyplayer, tidyteam, prop, projected_prob)

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
  # this gets us players whether they're playing or not, so can filter out the palyers who are not
  filter(!is.na(MATCHUP)) %>%
  # looks like the lineups can have duplicated players if someone is a tossup to start, so just keep the most informative row
  group_by(MATCHUP) %>%
  fill(tidyteam, .direction = 'updown') %>%
  mutate(teams = list(unique(tidyteam)),
         team1 = lapply(teams, '[[', 1),
         team2 = lapply(teams, '[[', 2),
         tidyopp = if_else(tidyteam == team1, team2, team1)) %>%
  ungroup() %>%
  group_by(tidyplayer) %>%
  arrange(desc(LINEUP_DESC)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  transmute(
    tidyplayer, tidyteam, tidyopp,
    home_away = HOME_AWAY,
    injury_status = TO_PLAY_DESC,
    starter_status = LINEUP_DESC)


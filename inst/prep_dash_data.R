# global ----
# libs
library(tidyverse)
load_all()
# timer
t1 <- Sys.time()
# vars for later
## used when combining props by type
prop_list <- c('fpts', 'ftts', 'points', 'assists', 'rebounds')
site_prefixes <- c('dk_', 'fd_', 'pb_')
# get props ----

# draftkings
dk_ftts <- try(get_props('dk', 'nba', 'ftts'))
dk_fpts <- try(get_props('dk', 'nba', 'fpts'))
dk_points_ou <- try(get_props('dk', 'nba', 'player points ou'))
dk_rebounds_ou <- try(get_props('dk', 'nba', 'player rebounds ou'))
dk_assists_ou <- try(get_props('dk', 'nba', 'player assists ou'))
dk_threes_ou <- try(get_props('dk', 'nba', 'player 3pts ou'))
# fanduel
fd_ftts <- try(get_props('fd', 'nba', 'ftts'))
fd_fpts <- try(get_props('fd', 'nba', 'fpts'))
fd_points_alt <- try(get_props('fd', 'nba', 'player points alt'))
fd_rebounds_alt <- try(get_props('fd', 'nba', 'player rebounds alt'))
fd_assists_alt <- try(get_props('fd', 'nba', 'player assists alt'))
fd_threes_alt <- try(get_props('fd', 'nba', 'player 3pts alt'))
fd_points_ou <- try(get_props('fd', 'nba', 'player points ou'))
fd_rebounds_ou <- try(get_props('fd', 'nba', 'player rebounds ou'))
fd_assists_ou <- try(get_props('fd', 'nba', 'player assists ou'))
fd_threes_ou <- try(get_props('fd', 'nba', 'player 3pts ou'))
fd_points_tiers <- try(get_props('fd', 'nba', 'player points tiers'))
fd_rebounds_tiers <- try(get_props('fd', 'nba', 'player rebounds tiers'))
fd_assists_tiers <- try(get_props('fd', 'nba', 'player assists tiers'))
fd_threes_tiers <- try(get_props('fd', 'nba', 'player 3pts tiers'))

# pointsbet
pb_ftts <- try(get_props('pb', 'nba', 'ftts'))
pb_fpts <- try(get_props('pb', 'nba', 'fpts'))
pb_points_alt <- try(get_props('pb', 'nba', 'player points alt'))

pb_points_ou <- try(get_props('pb', 'nba', 'player points ou'))
# pb_rebounds_ou <- try(get_props('pb', 'nba', 'player rebounds ou'))
# pb_assists_ou <- try(get_props('pb', 'nba', 'player assists ou'))
# pb_threes_ou <- try(get_props('pb', 'nba', 'player 3pts ou'))
pb_points_tiers <- try(get_props('pb', 'nba', 'player points tiers'))
# pb_rebounds_tiers <- try(get_props('pb', 'nba', 'player rebounds tiers'))
# pb_assists_tiers <- try(get_props('pb', 'nba', 'player assists tiers'))
# pb_threes_tiers <- try(get_props('pb', 'nba', 'player 3pts tiers'))

# get gambling_stuff data ----

# get fpts and fpts
first_team_to_score <- read.csv(
  'https://raw.githubusercontent.com/jimtheflash/gambling_stuff/main/data/02_curated/nba_first_to_score/first_team_to_score.csv.gz'
)
first_player_to_score <- read.csv(
  'https://raw.githubusercontent.com/jimtheflash/gambling_stuff/main/data/02_curated/nba_first_to_score/first_player_to_score.csv.gz'
)

# get the rosters for player-team info

rosters <- read.csv('/Users/jim/Documents/gambling_stuff/data/02_curated/nba_rosters/current.csv.gz') %>%
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
  'https://raw.githubusercontent.com/jimtheflash/gambling_stuff/main/data/02_curated/nba_lineups/rotowire.csv'
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

# create dashboard data ----
# purge the try-errors
try_errors <- Filter(function(x) 'try-error' %in% class(get(x)), ls())
message(Sys.time(), ' the following objects were removed as try-errors: ', paste(try_errors, collapse = ', '))
rm(list = try_errors)
# make a list of data.frames for each prop type in the props list
props_df_list <- list()
for (prop in prop_list) {
  # skip if there aren't any objects for that prop
  if (length(ls(pattern = prop)) == 0) {
    next
  }
  # stack everything into one big data.frame
  df_long <- do.call(rbind, lapply(ls(pattern = prop), get))
  # merge with rosters data to get teams for players
  if (!prop %in% c('ftts', 'first team to score')) df_long <- merge(df_long, rosters, all.x = TRUE)
  # add missing tidy columns
  if (!'tidyline' %in% names(df_long)) {
    df_long$tidyline <- NA_real_
  }
  if (!'tidyou' %in% names(df_long)) {
    df_long$tidyou <- NA_character_
  }
  if (!'tidyplayer' %in% names(df_long)) {
    df_long$tidyplayer <- NA_character_
  }
  # widen
  df_wide <- pivot_wider(
    data = df_long,
    id_cols = c(sport, prop, tidyplayer, tidyteam, tidyou, tidyline),
    names_from = site,
    values_from = tidyamericanodds
  )
  # fix prop names
  if (prop == 'fpts') prop <- 'first player to score'
  if (prop == 'ftts') prop <- 'first team to score'
  # store output
  props_df_list[[prop]] <- df_wide
}

# combine the list elements, and tidy up for dash output
props_df <- bind_rows(props_df_list) %>%
  mutate(
    # rename players in team-level props as 'team'
    tidyplayer = if_else(prop == 'first team to score', 'team', tidyplayer),
    # recode NAs in tidyou (KEEP IT THIS WAY, YOU CHANGED IT ONCE ALREADY AND IT WAS A BAD DECISION)
    tidyou = if_else(is.na(tidyou), 'N/A', tidyou)) %>%
  distinct()
# handle missing books
if (!'draftkings' %in% names(props_df)) props_df$draftkings <- NA_real_
if (!'fanduel' %in% names(props_df)) props_df$fanduel <- NA_real_
if (!'pointsbet' %in% names(props_df)) props_df$pointsbet <- NA_real_
props_df <- props_df %>%
  mutate(
    # pointsbet rounding else the best odds calc is jacked up
    pointsbet = round(pointsbet),
    # probability odds for other use
    dk_prob = bettoR::convert_odds(draftkings, output = 'prob'),
    fd_prob = bettoR::convert_odds(fanduel, output = 'prob'),
    pb_prob = bettoR::convert_odds(pointsbet, output = 'prob')
  ) %>%
  rowwise() %>%
  mutate(count_books = sum(!is.na(c(draftkings, fanduel, pointsbet))),
         mean_prob = mean(c(dk_prob, fd_prob, pb_prob), na.rm = TRUE),
         mean_odds = bettoR::convert_odds(mean_prob, input = 'prob', output = 'us'),
         worst_prob = max(c(dk_prob, fd_prob, pb_prob), na.rm = TRUE),
         best_prob = min(c(dk_prob, fd_prob, pb_prob), na.rm = TRUE),
         best_odds = bettoR::convert_odds(best_prob, input = 'prob', output = 'us'),
         probs_list = list(c(dk_prob, fd_prob, pb_prob)),
         odds_list = list(c(draftkings, fanduel, pointsbet))) %>%
  ungroup()

# add schedule data
props_df <- props_df %>%
  left_join(schedule)

# add projections
props_df <- props_df %>%
  left_join(first_player_to_score %>%
              mutate(fpts_projected_prob = projected_prob) %>%
              select(-projected_prob, -projected_line)) %>%
  left_join(first_team_to_score %>%
              mutate(ftts_projected_prob = projected_prob) %>%
              select(-tidyplayer, -projected_prob, -projected_line) %>%
              distinct()) %>%
  mutate(projected_prob = coalesce(ftts_projected_prob, fpts_projected_prob),
         dk_delta = projected_prob - dk_prob,
         fd_delta = projected_prob - fd_prob,
         pb_delta = projected_prob - pb_prob) %>%
  rowwise() %>%
  mutate(best_delta = max(c(dk_delta, fd_delta, pb_delta), na.rm = TRUE),
         best_delta = if_else(is.infinite(best_delta), NA_real_, best_delta)) %>%
  select(-ftts_projected_prob, -fpts_projected_prob) %>%
  ungroup()

# additional row-wise calculations
best_books <- list()
next_best_probs <- list()

for (i in 1:nrow(props_df)) {
  ## subset row
  sel_row <- props_df[i, ]
  ## get all the probabilities
  probs_vec <- na.omit(unlist(sel_row$probs_list))
  ### get the next best probabilities when there are more than 1 unique value
  next_best_probs[[length(next_best_probs) + 1]] <-
    ifelse(length(unique(probs_vec)) == 1, NA_real_, min(probs_vec[probs_vec > min(probs_vec)]))

  # get the best odds in a nicely formatted character string
  best <- sel_row$best_odds
  sel_row$mean_odds <- NULL
  sel_row$best_odds <- NULL
  sel_row$odds_list <- NULL
  bb <- sort(as.character(na.omit(names(sel_row)[unlist(sel_row) == best])))
  best_books[[length(best_books) + 1]] <- paste0(bb, collapse = ', ')

}
props_df <- props_df %>%
  select(-probs_list,
         -odds_list) %>%
  mutate(best_books = unlist(best_books),
         next_best_prob = unlist(next_best_probs),
         next_best_ratio = case_when(count_books == 1 ~ NA_real_,
                                     count_books == 2 ~ best_prob / worst_prob,
                                     count_books > 2 ~ best_prob / next_best_prob),
         projected_odds = bettoR::convert_odds(projected_prob, input = 'prob', output = 'us'))

# stash the datetime when these data were last updated
attr(props_df, 'timestamp') <- t1
# save dash output
saveRDS(props_df, 'inst/props.rds')

t2 <- Sys.time()
t2-t1

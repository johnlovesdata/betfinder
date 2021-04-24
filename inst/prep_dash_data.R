library(tidyverse)
load_all()

t1 <- Sys.time()

# get props ---------------------------------------------------------------
# draftkings
dk_ftts <- try(get_props('dk', 'nba', 'ftts'))
dk_fpts <- try(get_props('dk', 'nba', 'fpts'))
dk_points_ou <- try(get_props('dk', 'nba', 'player points ou'))
dk_rebounds_ou <- try(get_props('dk', 'nba', 'player rebounds ou'))
dk_assists_ou <- try(get_props('dk', 'nba', 'player assists ou'))
# fanduel
fd_ftts <- try(get_props('fd', 'nba', 'ftts'))
fd_fpts <- try(get_props('fd', 'nba', 'fpts'))
fd_points_alt <- try(get_props('fd', 'nba', 'player points alt'))
fd_rebounds_alt <- try(get_props('fd', 'nba', 'player rebounds alt'))
fd_assists_alt <- try(get_props('fd', 'nba', 'player assists alt'))
fd_points_ou <- try(get_props('fd', 'nba', 'player points ou'))
fd_rebounds_ou <- try(get_props('fd', 'nba', 'player rebounds ou'))
fd_assists_ou <- try(get_props('fd', 'nba', 'player assists ou'))
fd_points_tiers <- try(get_props('fd', 'nba', 'player points tiers'))
fd_rebounds_tiers <- try(get_props('fd', 'nba', 'player rebounds tiers'))
fd_assists_tiers <- try(get_props('fd', 'nba', 'player assists tiers'))
# pointsbet
pb_ftts <- try(get_props('pb', 'nba', 'ftts'))
pb_fpts <- try(get_props('pb', 'nba', 'fpts'))
pb_points_ou <- try(get_props('pb', 'nba', 'player points ou'))
pb_rebounds_ou <- try(get_props('pb', 'nba', 'player rebounds ou'))
pb_assists_ou <- try(get_props('pb', 'nba', 'player assists ou'))
pb_points_tiers <- try(get_props('pb', 'nba', 'player points tiers'))
pb_rebounds_tiers <- try(get_props('pb', 'nba', 'player rebounds tiers'))
pb_assists_tiers <- try(get_props('pb', 'nba', 'player assists tiers'))

# purge the try-errors
try_errors <- Filter(function(x) 'try-error' %in% class(get(x)), ls())
rm(list = try_errors)

# get the rosters for team info (for filtering at the very least)
rosters <- read.csv(
  '/Users/jim/Documents/gambling_stuff/data/02_curated/nba_rosters/current.csv.gz'
) %>%
  transmute(
    tidyplayer = normalize_names(
      PLAYER_NAME,
      key = system.file('lu', 'nba', 'player', 'lu.json', package = 'betfinder')
    ),
    tidyteam = normalize_names(
      TEAM_ABBREVIATION,
      key = system.file('lu', 'nba', 'team', 'lu.json', package = 'betfinder')
    ))

# make a list of data.frames for each main metric
df_list <- list()
for (metric in c('points', 'rebounds', 'assists', 'fpts', 'ftts')) {
  if (length(ls(pattern = metric)) == 0) {
    next
  }

  df_long <- do.call(rbind, lapply(ls(pattern = metric), get))

  df_long <- merge(df_long, rosters, all.x = TRUE)
  if (!'tidyline' %in% names(df_long)) {
    df_long$tidyline <- NA
  }
  if (!'tidyou' %in% names(df_long)) {
    df_long$tidyou <- NA
  }
  if (!'tidyplayer' %in% names(df_long)) {
    df_long$tidyplayer <- NA
  }

  df_wide <- pivot_wider(
    data = df_long,
    id_cols = c(sport, prop, tidyplayer, tidyteam, tidyou, tidyline),
    names_from = site,
    values_from = tidyamericanodds
  )

  if (metric == 'fpts') metric <- 'first player to score'
  if (metric == 'ftts') metric <- 'first team to score'
  df_list[[metric]] <- df_wide
}

# combine the list elements, and tidy up for dash output
stacked <- bind_rows(df_list) %>%
  mutate(tidyou = if_else(is.na(tidyou), 'N/A', tidyou),
         pointsbet = round(pointsbet)) %>%
  rowwise() %>%
  mutate(count_books = sum(!is.na(c(draftkings, fanduel, pointsbet))),
         mean_odds = mean(c(draftkings, fanduel, pointsbet), na.rm = TRUE),
         best_odds = min(c(draftkings, fanduel, pointsbet), na.rm = TRUE))


# find the best books for each bet
# vals <- list()
# for (i in 1:nrow(stacked)) {
#   sel_row <- stacked[i, ]
#   best <- sel_row$best_odds
#   sel_row$mean_odds <- NULL
#   sel_row$best_odds <- NULL
#   vals[[length(vals) + 1]] <- paste0(sort(names(sel_row)[grepl(best, sel_row)]), collapse = ', ')
# }
# stacked$best_books <- vals
# save dash output
saveRDS(stacked, 'inst/dash_data.rds')

t2 <- Sys.time()
t2-t1

# research data ----
gamelogs_path <- '/Users/jim/Documents/gambling_stuff/data/nba_gamelogs/'
gamelogs <- list.files(gamelogs_path, pattern = 'csv')
gamelogs_list <- list()
for (g in gamelogs) {
  res <- read.csv(file.path(gamelogs_path, g))
  gamelogs_list[[length(gamelogs_list) + 1]] <- res
}
gamelogs_df <- bind_rows(gamelogs_list) %>%
  filter(PLAYER_NAME %in% unique(player_data$tidyplayer))

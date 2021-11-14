#' get the main bets
#' @param sport only works for nba right now
#' @export
get_main_bets <- function(sport) {

  # get the raw nba data
  dk_nba <- get_draftkings_data(sport)
  fd_nba <- get_fanduel_data(sport)
  pb_nba <- get_pointsbet_data(sport)

  # parse out the main bets
  dk_main <- parse_draftkings_data(dk_nba, sport, game_lines = TRUE)
  fd_main <- parse_fanduel_data(fd_nba, sport, game_lines = TRUE)
  pb_main <- parse_pointsbet_data(pb_nba, game_lines = TRUE)

  # tidyup
  dk_tidy <- tidyup_draftkings_data(dk_main, sport, game_lines = TRUE)
  fd_tidy <- tidyup_fanduel_data(fd_main, sport, game_lines = TRUE)
  pb_tidy <- tidyup_pointsbet_data(pb_main, sport, game_lines = TRUE)

  # merge
  df_long <- dplyr::bind_rows(dk_tidy, fd_tidy, pb_tidy)

  return(df_long)

}

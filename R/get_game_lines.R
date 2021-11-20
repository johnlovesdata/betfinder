#' get the main bets
#' @param sport only works for nba right now
#' @export
get_game_lines <- function(sport) {

  # get the raw nba data
  dk_sport <- get_draftkings_data(sport, sleep_time = 0)
  fd_sport <- get_fanduel_data(sport, sleep_time = 0)
  pb_sport <- get_pointsbet_data(sport, sleep_time = 0)
  br_sport <- get_betrivers_data(sport, sleep_time = 0)

  # parse out the main bets
  dk_game_lines <- parse_draftkings_data(dk_sport, sport, game_lines = TRUE)
  fd_game_lines <- parse_fanduel_data(fd_sport, sport, game_lines = TRUE)
  pb_game_lines <- parse_pointsbet_data(pb_sport, game_lines = TRUE)
  br_game_lines <- parse_betrivers_data(br_sport, sport, game_lines = TRUE)

  # tidyup
  dk_tidy <- tidyup_draftkings_data(dk_game_lines, sport, game_lines = TRUE)
  fd_tidy <- tidyup_fanduel_data(fd_game_lines, sport, game_lines = TRUE)
  pb_tidy <- tidyup_pointsbet_data(pb_game_lines, sport, game_lines = TRUE)
  br_tidy <- tidyup_betrivers_data(br_game_lines, sport, game_lines = TRUE)

  # merge
  df_long <- dplyr::bind_rows(dk_tidy, fd_tidy, pb_tidy, br_tidy)

  return(df_long)

}

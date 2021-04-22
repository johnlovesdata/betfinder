#' Get lines on prop bets for popular sports from popular betting sites
#'
#' @param site \code{character} book site; support 'fanduel'/'fd', 'draftkings'/'dk', and 'pointsbet'/'pb'
#' @param sport \code{character} sport, e.g. 'nba'
#' @param prop \code{character} name of prop, e.g. 'first team to score'; see details for available props
#' @param save_path \code{character} path to save jsons, if not \code{NULL}
#' @details available props include: 'player points alt', 'player points ou', 'player points tiers', 'first team to score', 'first player to score'
#' @return \code{data.frame} tidy props with tidy columns
#' @export
get_props <- function(site, sport, prop, save_path = NULL) {

  # fix case errors for users
  site <- tolower(site)
  sport <- tolower(sport)
  prop <- tolower(prop)

  # for each site, for each sport, call the specific site wrappers
  if (site %in% c('dk', 'draftkings')) {
    dk_raw <- get_draftkings_data(sport = sport, save_path = save_path)
    dk_parsed <- parse_draftkings_data(dk_raw, prop = prop)
    output_df <- tidyup_draftkings_data(dk_parsed, sport = sport, prop = prop)
  }

  if (site %in% c('fd', 'fanduel')) {
    fd_raw <- get_fanduel_data(sport = sport, save_path = save_path)
    fd_parsed <- parse_fanduel_data(fd_raw, prop = prop)
    output_df <- tidyup_fanduel_data(fd_parsed, sport = sport, prop = prop)
  }

  if (site %in% c('pb', 'pointsbet')) {
    pb_raw <- get_pointsbet_data(sport = sport, save_path = save_path)
    pb_parsed <- parse_pointsbet_data(pb_raw, prop = prop)
    output_df <- tidyup_pointsbet_data(pb_parsed, sport = sport, prop = prop)
  }

  # append a timestamp and return
  output_df$timestamp <- Sys.time()
  return(output_df)

}

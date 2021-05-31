#' Get lines and odds on prop bets for popular sports from popular betting sites
#'
#' @param site \code{character} book site; support 'fanduel'/'fd', 'draftkings'/'dk', and 'pointsbet'/'pb'
#' @param sport \code{character} sport, e.g. 'nba'
#' @param prop \code{character} name of prop, e.g. 'first team to score'; see details for available props
#' @param save_path \code{character} path to save jsons, if not \code{NULL}
#' @details available props include: 'player points alt', 'player points ou', 'player points tiers', 'first team to score', 'first player to score'
#' @return \code{data.frame} tidy props with tidy columns
#' @name get_props
#' @export
get_props <- function(site, sport, prop, save_path = NULL) {

  # fix case errors for users
  site <- tolower(site)
  sport <- tolower(sport)
  prop <- tolower(prop)

  # for each site, for each sport, call the specific site wrappers
  if (site %in% c('dk', 'draftkings')) {
    dk_raw <- try(get_draftkings_data(sport = sport, save_path = save_path))
    if (!inherits(dk_raw, 'try-error')) dk_parsed <- try(parse_draftkings_data(dk_raw, prop = prop))
    if (!inherits(dk_parsed, 'try-error')) output_df <- try(tidyup_draftkings_data(dk_parsed, sport = sport, prop = prop))
  }

  if (site %in% c('fd', 'fanduel')) {
    fd_raw <- try(get_fanduel_data(sport = sport, sleep_time = .01, save_path = save_path))
    if (!inherits(fd_raw, 'try-error')) fd_parsed <- try(parse_fanduel_data(fd_raw, sport = sport, prop = prop))
    if (!inherits(fd_parsed, 'try-error')) output_df <- try(tidyup_fanduel_data(fd_parsed, sport = sport, prop = prop))
  }

  if (site %in% c('pb', 'pointsbet')) {
    pb_raw <- try(get_pointsbet_data(sport = sport, sleep_time = .01, save_path = save_path))
    if (!inherits(pb_raw, 'try-error')) pb_parsed <- try(parse_pointsbet_data(pb_raw, prop = prop))
    if (!inherits(pb_parsed, 'try-error')) output_df <- try(tidyup_pointsbet_data(pb_parsed, sport = sport, prop = prop))
  }

 if (!'output_df' %in% ls()) return()

  # append a timestamp and return
  output_df$timestamp <- Sys.time()
  return(output_df)

}

#' @rdname get_props
get_all_props <- function(sports = c('nba'),
                          sites = c('dk', 'fd', 'pb'),
                          props = c('ftts', 'fpts',
                          'player points alt', 'player points ou', 'player points tiers',
                          'player assists alt', 'player assists ou', 'player assists tiers',
                          'player rebounds alt', 'player rebounds ou', 'player rebounds tiers',
                          'player 3pts alt', 'player 3pts ou', 'player 3pts tiers'),
                          save_path = NULL) {
  # blow up the grid of sport, site, and prop
  args_df <- expand.grid(
    sport = sports,
    site = sites,
    prop = props,
    stringsAsFactors = FALSE)
  # loop through the massive set of arguments
  output_list <- list()
  for (i in 1:nrow(args_df)) {
    arg_row <- args_df[i, ]
    props <- try(get_props(sport = arg_row$sport,
                           site = arg_row$site,
                           prop = arg_row$prop))
    if (inherits(props, 'try-error')) next
    output_list[[length(output_list) + 1]] <- props
  }
  return(output_list)
}

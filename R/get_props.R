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
    dk_raw <- get_draftkings_data(sport = sport, sleep_time = .01, save_path = save_path)
    dk_parsed <- parse_draftkings_data(dk_raw, sport = sport, prop = prop)
    dk_tidy <- tidyup_draftkings_data(dk_parsed, sport = sport, prop = prop)
    output_df <- dk_tidy
    output_df$timestamp <- Sys.time()
    return(output_df)
  }

  if (site %in% c('fd', 'fanduel')) {
    fd_raw <- get_fanduel_data(sport = sport, sleep_time = .01, save_path = save_path)
    fd_parsed <- parse_fanduel_data(fd_raw, sport = sport, prop = prop)
    fd_tidy <- tidyup_fanduel_data(fd_parsed, sport = sport, prop = prop)
    output_df <- fd_tidy
    output_df$timestamp <- Sys.time()
    return(output_df)
  }

  if (site %in% c('pb', 'pointsbet')) {
    pb_raw <- get_pointsbet_data(sport = sport, sleep_time = .01, save_path = save_path)
    pb_parsed <- parse_pointsbet_data(pb_raw, prop = prop)
    pb_tidy <- tidyup_pointsbet_data(pb_parsed, sport = sport, prop = prop)
    output_df <- pb_tidy
    output_df$timestamp <- Sys.time()
    return(output_df)
  }
}


#' @rdname get_props
get_all_props <- function(props_path = system.file('config', 'props', 'props_list.rds', package = 'betfinder'),
                          save_path = NULL) {
  # blow up the grid of sport, site, and prop
  args_df <- readRDS(props_path)
  args_df <- args_df[args_df$active == 1, ]
  # loop through the massive set of arguments
  output_list <- list()
  for (i in 1:nrow(args_df)) {
    arg_row <- args_df[i, ]
    print(arg_row)
    props <- try(get_props(sport = arg_row$sport,
                           site = arg_row$site,
                           prop = arg_row$prop))
    if (is.null(props) || inherits(props, 'try-error') || !is.data.frame(props) || nrow(props) < 1) next
    output_list[[length(output_list) + 1]] <- props
  }
  return(output_list)
}

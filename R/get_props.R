#' Get lines and odds on prop bets for popular sports from popular betting sites
#'
#' @param site \code{character} book site; support 'fanduel'/'fd', 'draftkings'/'dk', 'pointsbet'/'pb', 'betrivers'/'br', 'barstool'/'bs', 'mgm', 'caesar'/'csr'
#' @param sport \code{character} sport, e.g. 'nba'
#' @param prop \code{character} name of prop, e.g. 'first team to score'; see details for available props
#' @param raw_data \code{data.frame} defaults to NULL, but you can pass your pre-existing data.frame to be parsed
#' @param save_path \code{character} path to save jsons, if not \code{NULL}
#' @details available props include: 'player points alt', 'player points ou', 'player points tiers', 'first team to score', 'first player to score'
#' @return \code{data.frame} tidy props with tidy columns
#' @name get_props
#' @export
get_props <- function(site, sport, prop, raw_data = NULL, save_path = NULL) {

  # fix case errors for users
  site <- tolower(site)
  sport <- tolower(sport)
  prop <- tolower(prop)

  # for each site, for each sport, call the specific site wrappers
  if (site %in% c('dk', 'draftkings')) {
    if (is.null(raw_data)) {
      dk_raw <- get_draftkings_data(sport = sport, sleep_time = .01, save_path = save_path)
    } else {
      dk_raw <- raw_data
      }
    dk_parsed <- parse_draftkings_data(dk_raw, sport = sport, prop = prop)
    dk_tidy <- tidyup_draftkings_data(dk_parsed, sport = sport, prop = prop)
    output_df <- dk_tidy
    output_df$timestamp <- Sys.time()
    return(output_df)
  }

  if (site %in% c('fd', 'fanduel')) {
    if (is.null(raw_data)) {
      fd_raw <- get_fanduel_data(sport = sport, sleep_time = .01, save_path = save_path)
    } else {
      fd_raw <- raw_data
    }
    fd_parsed <- parse_fanduel_data(fd_raw, sport = sport, prop = prop)
    fd_tidy <- tidyup_fanduel_data(fd_parsed, sport = sport, prop = prop)
    output_df <- fd_tidy
    output_df$timestamp <- Sys.time()
    return(output_df)
  }

  if (site %in% c('pb', 'pointsbet')) {
    if (is.null(raw_data)) {
      pb_raw <- get_pointsbet_data(sport = sport, sleep_time = .01, save_path = save_path)
    } else {
      pb_raw <- raw_data
    }
    pb_parsed <- parse_pointsbet_data(pb_raw, prop = prop)
    pb_tidy <- tidyup_pointsbet_data(pb_parsed, sport = sport, prop = prop)
    output_df <- pb_tidy
    output_df$timestamp <- Sys.time()
    return(output_df)
  }

  if (site %in% c('br', 'betrivers')) {
    if (is.null(raw_data)) {
      br_raw <- get_betrivers_data(sport = sport, sleep_time = .01, save_path = save_path)
    } else {
      br_raw <- raw_data
    }
    br_parsed <- parse_betrivers_data(br_raw, sport = sport, prop = prop)
    br_tidy <- tidyup_betrivers_data(br_parsed, sport = sport, prop = prop)
    output_df <- br_tidy
    output_df$timestamp <- Sys.time()
    return(output_df)
  }

  if (site %in% c('bs', 'barstool')) {
    if (is.null(raw_data)) {
      bs_raw <- get_barstool_data(sport = sport, sleep_time = .01, save_path = save_path)
    } else {
      bs_raw <- raw_data
    }
    bs_parsed <- parse_barstool_data(bs_raw, sport = sport, prop = prop)
    bs_tidy <- tidyup_barstool_data(bs_parsed, sport = sport, prop = prop)
    output_df <- bs_tidy
    output_df$timestamp <- Sys.time()
    return(output_df)
  }

  if (site %in% c('mgm')) {
    if (is.null(raw_data)) {
      mgm_raw <- get_mgm_data(sport = sport, sleep_time = .01, save_path = save_path)
    } else {
      mgm_raw <- raw_data
    }
    mgm_parsed <- parse_mgm_data(mgm_raw, sport = sport, prop = prop)
    mgm_tidy <- tidyup_mgm_data(mgm_parsed, sport = sport, prop = prop)
    output_df <- mgm_tidy
    output_df$timestamp <- Sys.time()
    return(output_df)
  }
}


#' @rdname get_props
get_all_props <- function(
  props_path = system.file('config', 'props', 'props_list.rds', package = 'betfinder'),
  save_path = NULL
  ) {
  # read in the list of props
  args_df <- readRDS(props_path)
  args_df <- args_df[args_df$active == 1, ]
  # get the unique combination of sites and sports so we know which raw data needs to be pulled
  site_sports <- unique(args_df[, c('sport', 'site')])
  raw_data_list <- list()
  for (i in 1:nrow(site_sports)) {
    sel_row <- site_sports[i, ]
    message('getting ', sel_row$site, ' ', sel_row$sport)
    if (sel_row$site == 'dk') {
      raw_data_list[['dk']][[sel_row$sport]] <- get_draftkings_data(sel_row$sport)
    }
    if (sel_row$site == 'fd') {
      raw_data_list[['fd']][[sel_row$sport]] <- get_fanduel_data(sel_row$sport)
    }
    if (sel_row$site == 'pb') {
      raw_data_list[['pb']][[sel_row$sport]] <- get_pointsbet_data(sel_row$sport)
    }
  }
  # loop through the massive set of arguments and get what we need from the raw data
  output_list <- list()
  for (i in 1:nrow(args_df)) {
    arg_row <- args_df[i, ]
    message('parsing ', arg_row$site, ' ', arg_row$sport, ' ', arg_row$prop)
    raw_data_to_use <- raw_data_list[[arg_row$site]][[arg_row$sport]]
    props <- try(get_props(sport = arg_row$sport,
                           site = arg_row$site,
                           prop = arg_row$prop,
                           raw_data = raw_data_to_use))
    if (is.null(props) || inherits(props, 'try-error') || !is.data.frame(props) || nrow(props) < 1) next
    output_list[[length(output_list) + 1]] <- props
  }
  return(output_list)
}

#' Get lines and odds on main bets for popular sports from popular betting sites
#'
#' @param site \code{character} book site; support 'fanduel'/'fd', 'draftkings'/'dk', 'pointsbet'/'pb', 'betrivers'/'br', 'barstool'/'bs', 'mgm', 'caesar'/'csr'
#' @param sport \code{character} sport, e.g. 'nba'
#' @param exclude_live \code{boolean} should live lines be excluded
#' @param exclude_alts \code{boolean} should alternate lines be excluded
#' @param raw_data \code{data.frame} defaults to NULL, but you can pass your pre-existing data.frame to be parsed
#' @param save_path \code{character} path to save jsons, if not \code{NULL}
#' @details this code should pull moneylines, spreads, and totals, including alternate spreads and totals,
#' @return \code{data.frame} tidy game lines with tidy columns
#' @export
get_game_lines <- function(site, sport, exclude_live = TRUE, exclude_alts = FALSE, raw_data = NULL, save_path = NULL) {

  # fix case errors for users
  site <- tolower(site)
  sport <- tolower(sport)

  # for each site, for each sport, call the specific site wrappers
  if (site %in% c('dk', 'draftkings')) {
    if (is.null(raw_data)) {
      dk_raw <- get_draftkings_data(sport = sport, sleep_time = .01, save_path = save_path)
    } else {
      dk_raw <- raw_data
    }
    dk_parsed <- parse_draftkings_data(dk_raw, sport = sport, game_lines = TRUE, exclude_live = exclude_live, exclude_alts = exclude_alts)
    dk_tidy <- tidyup_draftkings_data(dk_parsed, sport = sport, game_lines = TRUE)
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
    fd_parsed <- parse_fanduel_data(fd_raw, sport = sport, game_lines = TRUE, exclude_live = exclude_live, exclude_alts = exclude_alts)
    fd_tidy <- tidyup_fanduel_data(fd_parsed, sport = sport, game_lines = TRUE)
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
    pb_parsed <- parse_pointsbet_data(pb_raw, game_lines = TRUE, exclude_live = exclude_live, exclude_alts = exclude_alts)
    pb_tidy <- tidyup_pointsbet_data(pb_parsed, sport = sport, game_lines = TRUE)
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
    br_parsed <- parse_betrivers_data(br_raw, sport = sport, game_lines = TRUE)
    br_tidy <- tidyup_betrivers_data(br_parsed, sport = sport, game_lines = TRUE)
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
    bs_parsed <- parse_barstool_data(bs_raw, sport = sport, game_lines = TRUE, exclude_live = exclude_live, exclude_alts = exclude_alts)
    bs_tidy <- tidyup_barstool_data(bs_parsed, sport = sport, game_lines = TRUE)
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
    mgm_parsed <- parse_mgm_data(mgm_raw, sport = sport, game_lines = TRUE)
    mgm_tidy <- tidyup_mgm_data(mgm_parsed, sport = sport, game_lines = TRUE)
    output_df <- mgm_tidy
    output_df$timestamp <- Sys.time()
    return(output_df)
  }
}

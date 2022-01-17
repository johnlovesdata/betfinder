#' Get sportsbook data
#'
#' @param sportsbook character name of sports book
#' @param sport character sport, which is really the league abbreviation
#' @param save_rds logical save off an RDS?
#' @param save_path character where to save the file, which takes the format "save_path/sportsbook_sport_datetime.rds"
#'
#' @return list
#' @export

get_sportsbook_data <- function(sportsbook = NA_character_,
                                sport = NA_character_,
                                save_rds = FALSE,
                                save_path = '~/') {

  # execute the correct get call for the sportsbook and sport
  if (is.na(sport)) stop('missing sport arg')
  else if (sportsbook == 'betrivers') sportsbook_data <- get_betrivers_data(sport = sport)
  else if (sportsbook == 'draftkings') sportsbook_data <- get_draftkings_data(sport = sport)
  else if (sportsbook == 'fanduel') sportsbook_data <- get_fanduel_data(sport = sport)
  else if (sportsbook == 'pointsbet') sportsbook_data <- get_pointsbet_data(sport = sport)
  else stop('either a missing sportsbook or a sportsbook that we have not yet added')

  # save the raw data if desired
  if (save_rds) {
    rds_fn <- paste(save_path, sportsbook, sport, paste0(as.numeric(Sys.time()), '.rds'), collapse = '_')
    saveRDS(sportsbook_data, file = rds_fn)
  }

  # return it
  return(sportsbook_data)

}

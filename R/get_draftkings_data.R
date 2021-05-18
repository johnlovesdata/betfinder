get_draftkings_data <- function(sport, save_path = NULL) {
  # set the eventgroup based on the sport
  if (sport == 'nba') eg <- 103
  else if (sport == 'nhl') eg <- 2022
  else stop(sport, ' is not yet supported')
  # get the big ol' json from dk - this has all the nba markets
  main_uri <- paste0('https://gaming-us-il.draftkings.com//sites/US-IL-SB/api/v1/eventgroup/', eg, '/full')
  main_query <- list(
    includePromotions = list('true'),
    format = list('json')
  )
  main_content <- get_content(main_uri, main_query)
  # save it off if there's a save_path
  if (!is.null(save_path)) {
    fn <- paste0(sport, '_draftkings_', as.numeric(Sys.time()), '.json')
    jsonlite::write_json(main_content, file.path(save_path, fn))
    R.utils::gzip(file.path(save_path, fn), ext='gz')
  }
  # return
  return(main_content)
}

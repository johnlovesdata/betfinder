get_draftkings_data <- function(sport, save_path = NULL,
                                sleep_time = 0) {
  # set the eventgroup based on the sport
  if (sport == 'nba') eg <- 88670846
  else if (sport == 'mlb') eg <- 88670847
  else if (sport == 'nhl') eg <- 88670853
  else if (sport == 'nfl') eg <- 88670561
  else if (sport == 'ncaaf') eg <- 88670775
  else stop(sport, ' is not yet supported')

  # get the big ol' json from dk - this has all the nba markets
  main_uri <- paste0('https://sportsbook-us-il.draftkings.com//sites/US-IL-SB/api/v4/eventgroups/', eg)
  main_query <- list(
    includePromotions = list('true'),
    format = list('json')
  )
  main_content <- get_content(main_uri, main_query)
  # get a list of all the events that we could bet on for that sport
  event_ids <- unlist(lapply(main_content$eventGroup$events, '[[', 'eventId'))

  event_list <- list()
  for (e in event_ids) {
    Sys.sleep(sleep_time)
    event_uri <- paste0('https://sportsbook-us-il.draftkings.com//sites/US-IL-SB/api/v2/event/', e)
    event_query <- list(
      includePromotions = list('true'),
      format = list('json'))
    game_event <- get_content(event_uri, event_query)
    event_list[[as.character(e)]] <- game_event

    if (!is.null(save_path)) {
      fn <- paste0(sport, '_draftkings_', e, '_', as.numeric(Sys.time()), '.json')
      jsonlite::write_json(game_event, file.path(save_path, fn))
      R.utils::gzip(file.path(save_path, fn), ext='gz')
    }
  }

  # save it off if there's a save_path
  if (!is.null(save_path)) {
    fn <- paste0(sport, '_draftkings_', as.numeric(Sys.time()), '.json')
    jsonlite::write_json(main_content, file.path(save_path, fn))
    R.utils::gzip(file.path(save_path, fn), ext='gz')
  }

  # return
  return(event_list)
}

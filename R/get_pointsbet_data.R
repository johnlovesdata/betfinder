get_pointsbet_data <- function(sport, save_path = NULL, sleep_time = runif(1, 1.6, 3.2)) {

  # pointsbet is similar to fanduel - one main sport json, then event-specific jsons
  main_json <-
    jsonlite::fromJSON(
      'https://api.il.pointsbet.com/api/v2/competitions/5/events/featured?includeLive=false&page=1'
    )
  event_ids <- main_json$events$key

  # loop through the event_ids
  event_list <- list()
  for (e in event_ids) {
    # sleep to be nice
    Sys.sleep(sleep_time)
    # make the json string using the event_id, then grab the json, and pull the right prop
    json_string <-
      paste0('https://api.il.pointsbet.com/api/v2/events/', e)
    game_event <- jsonlite::fromJSON(json_string)
    event_list[[e]] <- game_event

    if (!is.null(save_path)) {
      fn <- paste0(sport, '_pointsbet_', e, '_', as.numeric(Sys.time()), '.json')
      jsonlite::write_json(game_event, file.path(save_path, fn))
    }
  }

  return(event_list)


}


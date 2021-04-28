get_pointsbet_data <- function(sport, save_path = NULL,
                               sleep_time = runif(1, 1.6, 3.2)) {
  if (sport == 'nba') {
    main_uri <- 'https://api.il.pointsbet.com/api/v2/competitions/5/events/featured'
    main_query <- list(
      includeLive = list('false'),
      page = list(1)
    )
    main_content <- get_content(main_uri, main_query)
    event_ids <- unlist(lapply(main_content$events, '[[', 'key'))
    # loop through the event_ids and get event-level (game-specific) jsons
    event_list <- list()
    for (e in event_ids) {
      # sleep to be nice
      Sys.sleep(sleep_time)
      # make the json string using the event_id, then grab the json, and pull the right prop
      event_URI <- paste0('https://api.il.pointsbet.com/api/v2/events/', e)
      event_content <- get_content(uri = event_URI, query = NULL)
      event_list[[e]] <- event_content

    if (!is.null(save_path)) {
      fn <- paste0(sport, '_pointsbet_', e, '_', as.numeric(Sys.time()), '.json')
      jsonlite::write_json(game_event, file.path(save_path, fn))
    }
    }

    return(event_list)
  }
}



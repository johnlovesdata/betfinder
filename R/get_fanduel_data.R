get_fanduel_data <- function(sport, save_path = NULL, sleep_time = runif(1, 1.6, 3.2)) {

  if (sport == 'nba') {

    # fanduel requires you to grab a main json for the day's games, then separate json's for all the props in a specific game
    main_json <-
      jsonlite::fromJSON('https://il.sportsbook.fanduel.com/cache/psmg/UK/63747.3.json')
    event_ids <- as.character(main_json$events$idfoevent)

    # loop through the event_ids and make a list of jsons
    event_list <- list()
    for (e in event_ids) {
      # sleep to be nice
      Sys.sleep(sleep_time)
      # make the json string using the event_id, then grab the json, and pull the right prop
      json_string <-
        paste0(
          'https://il.sportsbook.fanduel.com/cache/psevent/UK/1/false/',
          e,
          '.json'
        )
      game_event <- jsonlite::fromJSON(json_string)

      event_list[[e]] <- game_event

      if (!is.null(save_path)) {
        fn <- paste0(sport, '_fanduel_', e, '_', as.numeric(Sys.time()), '.json')
        jsonlite::write_json(game_event, file.path(save_path, fn))
      }
    }

    # return as a list of lists (yikes!)
    return(event_list)
  }

}

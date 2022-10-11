get_caesars_data <- function(sport, save_path = NULL, sleep_time = 0) {

  # get all the events, and index by sportId
  main_URI <- 'https://www.williamhill.com/us/il/bet/api/v3/events/highlights/?promotedOnly=true'
  main_query <- list()
  main_content <- get_content(main_URI, main_query)
  sport_index <- unlist(lapply(main_content, '[[', 'sportId'))

  if (sport == 'nfl') {
    sport_element <- main_content[[which(sport_index == 'americanfootball')]]
    competition_index <- unlist(lapply(sport_element$competitions, '[[', 'name'))
    competition <- sport_element$competitions[[which(competition_index == 'NFL')]]
  }

  event_name_list <- list()
  for (i in competition$events) {
    event_name_row <- data.frame(
      event_id = i[['id']],
      event_name = i[['name']]
    )
    event_name_list[[length(event_name_list) + 1]] <- event_name_row
  }
  event_name_df <- do.call(rbind, event_name_list)
  event_ids <- unique(event_name_df$event_id)

  # loop through the event_ids and get event-level (game-specific) jsons
  event_list <- list()
  for (e in event_ids) {
    # sleep to be nice
    Sys.sleep(sleep_time)
    # make the json string using the event_id, then grab the json
    event_URI <- paste0('https://www.williamhill.com/us/il/bet/api/v3/events/', e)
    event_query <- list()
    event_content <- get_content(uri = event_URI, query = event_query)
    event_list[[as.character(e)]] <- event_content

    if (!is.null(save_path)) {
      fn <- paste0(sport, '_caesars_', e, '_', as.numeric(Sys.time()), '.json')
      jsonlite::write_json(game_event, file.path(save_path, fn))
      R.utils::gzip(file.path(save_path, fn), ext='gz')
    }
  }

  # return as a list of lists (yikes!)
  return(event_list)
}

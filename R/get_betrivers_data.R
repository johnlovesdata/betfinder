get_betrivers_data <- function(sport, save_path = NULL, sleep_time = runif(1, 1.6, 3.2)) {

  # set the group_id_code by sport
  if (sport == 'nba') group_id_code <- 1000093652
  main_URI <- 'https://il.betrivers.com/api/service/sportsbook/offering/listview/events'
  # set the query params
  main_query <- list(t = list(""),
                     pageNr = list(""),
                     cageCode = list("847"),
                     groupId = list(group_id_code))
  main_content <- get_content(main_URI, main_query)
  event_name_list <- list()
  for (i in main_content$items) {
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
    # make the json string using the event_id, then grab the json, and pull the right prop
    event_URI <- 'https://il.betrivers.com/api/service/sportsbook/offering/listview/details'
    event_query <- list(t = list(""),
                        cageCode = list("847"),
                        eventId = list(e))
    event_content <- get_content(uri = event_URI, query = event_query)
    event_list[[as.character(e)]] <- event_content

    if (!is.null(save_path)) {
      fn <- paste0(sport, '_betrivers_', e, '_', as.numeric(Sys.time()), '.json')
      jsonlite::write_json(game_event, file.path(save_path, fn))
      R.utils::gzip(file.path(save_path, fn), ext='gz')
    }
  }

  # return as a list of lists (yikes!)
  return(event_list)
}

get_barstool_data <- function(sport, save_path = NULL, sleep_time = runif(1, 1.6, 3.2)) {

  # set the main_URI by sport
  if (sport == 'nba') main_URI <- 'https://eu-offering.kambicdn.org/offering/v2018/pivusil/listView/basketball/nba/all/all/matches.json'
  # set the query params
  main_query <- list(market = list("US"),
                     includeParticipants = list("true"),
                     useCombined = list("true"),
                     lang = list("en_US"))
  main_content <- get_content(main_URI, main_query)
  event_name_list <- list()
  for (i in main_content$events) {
    event_name_row <- data.frame(
      event_id = i$event[['id']],
      event_name = i$event[['name']]
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
    event_URI <- paste0('https://eu-offering.kambicdn.org/offering/v2018/pivusil/betoffer/event/', e)
    event_query <- list(market = list("US"),
                        type = list(""),
                        range_start = list("0"),
                        range_size = list("0"),
                        includeParticipants = list("true"),
                        useCombined = list("true"),
                        lang = list("en_US"))
    event_content <- get_content(uri = event_URI, query = event_query)
    event_list[[as.character(e)]] <- event_content

    if (!is.null(save_path)) {
      fn <- paste0(sport, '_barstool_', e, '_', as.numeric(Sys.time()), '.json')
      jsonlite::write_json(game_event, file.path(save_path, fn))
      R.utils::gzip(file.path(save_path, fn), ext='gz')
    }
  }

  # return as a list of lists (yikes!)
  return(event_list)
}

get_mgm_data <- function(sport, save_path = NULL, sleep_time = runif(1, 1.6, 3.2)) {

  # set the main_URI by sport
  if (sport == 'nba') {
    sport_path <- '/Users/jim/Documents/gambling_stuff/mgm_jsons/'
  } else {
    stop('sport not supported')
  }

  # list the events
  event_file_list <- list.files(sport_path)

  # loop through those files and output some lists of event lists
  event_list <- list()
  for (e in event_file_list) {
    fn <- paste0(sport_path, e)
    game_event <- jsonlite::fromJSON(fn, flatten = TRUE)
    event_list[[length(event_list) + 1]] <- game_event

    if (!is.null(save_path)) {
      fn <- paste0(sport, '_mgm_', e, '_', as.numeric(Sys.time()), '.json')
      jsonlite::write_json(game_event, file.path(save_path, fn))
      R.utils::gzip(file.path(save_path, fn), ext='gz')
    }
  }

  # return as a list of lists (yikes!)
  return(event_list)
}

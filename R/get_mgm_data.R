get_mgm_data <- function(sport, save_path = NULL, sleep_time = 0) {

  # set the main_URI by sport
  if (sport == 'nba') {
    git_trees <- jsonlite::fromJSON('https://api.github.com/repos/jimtheflash/gambling_stuff/git/trees/main?recursive=1')
    tree <- git_trees$tree
    blobs <- tree[tree$type == 'blob', ]
    path_df <- blobs[grepl('data/01_raw/mgm_jsons', blobs$path), ]
    paths <- path_df$path

  } else {
    stop('sport not supported')
  }
  # loop through those files and output some lists of event lists
  event_list <- list()
  for (p in paths) {
    game_event <- jsonlite::fromJSON(paste0('https://raw.githubusercontent.com/jimtheflash/gambling_stuff/main/', p), flatten = TRUE)
    event_list[[length(event_list) + 1]] <- game_event

    if (!is.null(save_path)) {
      e <- gsub('01_raw', '', p)
      e <- gsub('[^0-9]', '', e)
      fn <- paste0(sport, '_mgm_', e, '_', as.numeric(Sys.time()), '.json')
      jsonlite::write_json(game_event, file.path(save_path, fn))
      R.utils::gzip(file.path(save_path, fn), ext='gz')
    }
  }

  # return as a list of lists (yikes!)
  return(event_list)
}

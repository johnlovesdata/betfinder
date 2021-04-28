get_draftkings_data <- function(sport, save_path = NULL) {
  if (sport == 'nba') {
    # get the big ol' json from dk - this has all the nba markets
    main_uri <- 'https://gaming-us-il.draftkings.com//sites/US-IL-SB/api/v1/eventgroup/103/full'
    main_query <- list(
      includePromotions = list('true'),
      format = list('json')
    )
    main_content <- get_content(main_uri, main_query)
    if (!is.null(save_path)) {
      fn <- paste0(sport, '_draftkings_', as.numeric(Sys.time()), '.json')
      jsonlite::write_json(main_content, file.path(save_path, fn))
    }
    return(main_content)
  } else {
    stop(sport, ' is not supported...YET')
  }
}

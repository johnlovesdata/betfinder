get_draftkings_data <- function(sport, save_path = NULL) {

  if (sport == 'nba') {
    # get the big ol' json from dk - this has all the nba markets
    ## PUT THIS IN A CONFIG ASSHOLES
    nba_markets <-
      jsonlite::fromJSON(
        'https://gaming-us-il.draftkings.com//sites/US-IL-SB/api/v1/eventgroup/103/full?includePromotions=true&format=json'
      )

    if (!is.null(save_path)) {
      fn <- paste0(sport, '_draftkings_', as.numeric(Sys.time()), '.json')
      jsonlite::write_json(nba_markets, file.path(save_path, fn))
    }

    return(nba_markets)

  } else {
    stop(sport, ' is not supported...YET')
  }

}

get_fanduel_data <- function(sport, save_path = NULL,
                             mlb_tabs = c('hits-runs', 'same-game-parlay-', 'innings'),
                             nba_tabs = c('player-points', 'player-rebounds', 'player-assists', 'player-threes',
                                      'player-combos', '1st-quarter', '2nd-quarter', '3rd-quarter', '4th-quarter'),
                             nfl_tabs = c('player-props', 'same-game-parlay-', '1st-half', '2nd-half',
                                          '1st-quarter', '2nd-quarter', '3rd-quarter', '4th-quarter'),
                             nhl_tabs = c('player-goals', 'player-points', 'player-assists', 'player-shots', 'goalie-props'),
                             sleep_time = runif(1, 1.6, 3.2)) {

  # fanduel requires you to grab a main json for the day's games, then separate json's for all the props in a specific game
  main_URI <- 'https://sbapi.il.sportsbook.fanduel.com/api/content-managed-page'
  # set the query params
  ## TODO: STRIP OUT MORE IF POSSIBLE
  ## TODO: FIGURE OUT WTF THAT _ak THING IS
  main_query <- list(betexRegion = list("GBR"),
                     capiJurisdiction = list("intl"),
                     currencyCode = list("USD"),
                     exchangeLocale = list("en_US"),
                     language = list("en"),
                     regionCode = list("NAMERICA"),
                     `_ak` = list("FhMFpcPWXMeyZxOx"),
                     page = list("CUSTOM"),
                     customPageId = list(sport))
  main_content <- get_content(main_URI, main_query)
  event_ids <- names(main_content$attachments$events)

  # loop through the event_ids and get event-level (game-specific) jsons
  event_list <- list()
  for (e in event_ids) {
    # sleep to be nice
    Sys.sleep(sleep_time)
    # make the json string using the event_id, then grab the json, and pull the right prop
    event_URI <- 'https://sbapi.il.sportsbook.fanduel.com/api/event-page'
    event_query <- list(betexRegion = list("GBR"),
                        capiJurisdiction = list("intl"),
                        currencyCode = list("USD"),
                        exchangeLocale = list("en_US"),
                        includePrices = list('true'),
                        language = list("en"),
                        priceHistory = list(1),
                        regionCode = list("NAMERICA"),
                        `_ak` = list("FhMFpcPWXMeyZxOx"),
                        eventId = list(e))
    event_content <- get_content(uri = event_URI, query = event_query)

    ## BUT WAIT THERES MORE - gotta grab each of the tabs for the specific props and stuff
    if (sport == 'mlb') tabs <- mlb_tabs
    else if (sport == 'nba') tabs <- nba_tabs
    else if (sport == 'nfl') tabs <- nfl_tabs
    else if (sport == 'nhl') tabs <- nhl_tabs
    else stop('unupported sport')

    tab_list <- list()
    for (i in tabs) {
      new_q <- event_query
      new_q$tab <- list(i)
      new_content <- get_content(uri = event_URI, query = new_q)
      if (identical(new_content, main_content)) {
        rm(new_content)
        next
      }
      tab_name <- gsub('-', '_', i)
      tab_list[[tab_name]] <- new_content
    }
    ### stitch together the content objects into a list
    content_list <- tab_list
    content_list$main <- event_content
    event_list[[e]] <- content_list

    if (!is.null(save_path)) {
      fn <- paste0(sport, '_fanduel_', e, '_', as.numeric(Sys.time()), '.json')
      jsonlite::write_json(game_event, file.path(save_path, fn))
      R.utils::gzip(file.path(save_path, fn), ext='gz')
    }

  }

  # return as a list of lists (yikes!)
  return(event_list)

}

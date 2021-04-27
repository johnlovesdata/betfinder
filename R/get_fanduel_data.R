get_fanduel_data <- function(sport, save_path = NULL,
                             sleep_time = runif(1, 1.6, 3.2)) {

  if (sport == 'nba') {

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
                       customPageId = list("nba"))
    resp <- httr::GET(url = main_URI, query = main_query, encode = 'json')
    main_content <- httr::content(resp)
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
      resp <- httr::GET(url = event_URI, query = event_query, encode = 'json')
      event_content <- httr::content(resp)

      ## BUT WAIT THERES MORE - gotta grab each of the tabs for the specific props and stuff (this is a pain in the ass unless i can pass a list to the tab parameter?)
      ### first quarter
      q1_query <- event_query
      q1_query$tab <- list('1st-quarter')
      resp <- httr::GET(url = event_URI, query = q1_query, encode = 'json')
      q1_content <- httr::content(resp)
      ### player points
      ppts_query <- event_query
      ppts_query$tab <- list('player-points')
      resp <- httr::GET(url = event_URI, query = ppts_query, encode = 'json')
      ppts_content <- httr::content(resp)
      ### player assists
      pasts_query <- event_query
      pasts_query$tab <- list('player-assists')
      resp <- httr::GET(url = event_URI, query = pasts_query, encode = 'json')
      pasts_content <- httr::content(resp)
      ### player rebounds
      prebs_query <- event_query
      prebs_query$tab <- list('player-rebounds')
      resp <- httr::GET(url = event_URI, query = prebs_query, encode = 'json')
      prebs_content <- httr::content(resp)

      ### stitch together the content objects into a list
      content_list <- list(
        main = event_content,
        first_quarter = q1_content,
        player_points = ppts_content,
        player_assists = pasts_content,
        player_rebounds = prebs_content
      )
      event_list[[e]] <- content_list

    #   if (!is.null(save_path)) {
    #     fn <- paste0(sport, '_fanduel_', e, '_', as.numeric(Sys.time()), '.json')
    #     jsonlite::write_json(game_event, file.path(save_path, fn))
    #   }

    }

    # return as a list of lists (yikes!)
    return(event_list)
  }

}

get_props <- function(site, sport, prop) {

  # fix case errors for users
  site <- tolower(site)
  sport <- tolower(sport)
  prop <- tolower(prop)

  # for each site, for each sport, make a subsection (for now...)
  if (site %in% c('fd', 'fanduel')) {

    if (sport == 'nba') {

      # fanduel requires you to grab a main json for the day's games, then separate json's for all the props in a specific game
      main_json <- jsonlite::fromJSON('https://il.sportsbook.fanduel.com/cache/psmg/UK/63747.3.json')
      event_ids <- as.character(main_json$events$idfoevent)

      if (prop %in% c('first team to score', 'ftts')) {

        # loop through the event_ids
        output_list <- list()
        for (e in event_ids) {
          # sleep to be nice
          Sys.sleep(runif(1, 1, 2))
          # make the json string using the event_id, then grab the json, and pull the right prop
          json_string <- paste0('https://il.sportsbook.fanduel.com/cache/psevent/UK/1/false/', e, '.json')
          game_event <- jsonlite::fromJSON(json_string)
          game_event_market_groups <- game_event$eventmarketgroups
          all_props <- game_event_market_groups$markets[game_event_market_groups$name == 'All'][[1]]
          first_team <- all_props$selections[all_props$name == 'Team to Score First'][[1]]
          first_team$description <- game_event$externaldescription
          output_list[[length(output_list) + 1]] <- first_team
        }
        output_df <- do.call(rbind, output_list)
      }

      if (prop %in% c('first player to score', 'fpts')) {

        # loop through the event_ids
        output_list <- list()
        for (e in event_ids) {
          # sleep to be nice
          Sys.sleep(runif(1, 1.6, 3.2))
          # make the json string using the event_id, then grab the json, and pull the right prop
          json_string <- paste0('https://il.sportsbook.fanduel.com/cache/psevent/UK/1/false/', e, '.json')
          game_event <- jsonlite::fromJSON(json_string)
          game_event_market_groups <- game_event$eventmarketgroups
          player_props <- game_event_market_groups$markets[game_event_market_groups$name == 'All Player Props'][[1]]
          first_basket <- player_props$selections[player_props$name == 'First Basket'][[1]]
          first_basket$description <- game_event$externaldescription
          output_list[[length(output_list) + 1]] <- first_basket
        }

        output_df <- do.call(rbind, output_list)
      }
    }
  }

  if (site %in% c('dk', 'draftkings')) {

    if (sport == 'nba') {

      # get the big ol' json from dk - this has all the nba markets
      nba_markets <- jsonlite::fromJSON('https://gaming-us-il.draftkings.com//sites/US-IL-SB/api/v1/eventgroup/103/full?includePromotions=true&format=json')
      # break out the offer markets
      offer_categories <- nba_markets$eventGroup$offerCategories

      if (prop %in% c('first team to score', 'ftts')) {

        # get game props loop through the games to make the output
        game_props <- offer_categories[offer_categories$name == "Game Props", ]$offerSubcategoryDescriptors[[1]]
        first_team_to_score <- game_props[game_props$offerSubcategory == "First Team to Score", ]$offerSubcategory$offers[[1]]
        outcomes <- do.call(rbind, first_team_to_score)$outcomes
        ## loop through the outcomes to make a list of data.frames to be combined
        output_list <- list()
        for (o in outcomes) {
          d <- data.frame(
            team = o$label,
            odds = o$oddsAmerican,
            opponent = rev(o$label)
          )
          output_list[[length(output_list) + 1]] <- d
        }
        output_df <- do.call(rbind, output_list)

      }

      if (prop %in% c('first player to score', 'fpts')) {

        player_props <- offer_categories[offer_categories$name == 'Player Props', ]$offerSubcategoryDescriptors[[1]]$offerSubcategory
        ffg <- player_props$offers[player_props$name == 'First Field Goal'][[1]]

        output_list <- list()
        for (i in 1:length(ffg)) {
          outcomes <- ffg[[i]]$outcomes[[1]]
          output_list[[i]] <- outcomes
        }
      }
    }
  }

  # return output as a df (probably need to do this within each call if we want output to be common)
  output_df <- do.call(rbind, output_list)
  return(output_df)
}

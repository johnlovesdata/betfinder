parse_pointsbet_data <- function(pointsbet_data, prop) {

  # loop through the pointsbet events to extract props
  output_list <- list()
  for (game_event in pointsbet_data) {

    # check for fixed odds markets, skip if they're not there
    if (!'fixedOddsMarkets' %in% names(game_event)) {
      next
    } else {
      fixed_odds_markets <- game_event$fixedOddsMarkets
    }

    # now extract correct props
    if (prop %in% c('first team to score', 'ftts')) {

      # skip if there's no ftts
      if (!'First Team to Score' %in% fixed_odds_markets$eventName) {
        next
        } else {
          first_team_to_score <-
            fixed_odds_markets$outcomes[fixed_odds_markets$eventName == 'First Team to Score'][[1]]
        }

      output_list[[length(output_list) + 1]] <- first_team_to_score
    }

    if (prop %in% c('first player to score', 'fpts')) {

      # skip if no first basket props
      if (!'First Basket' %in% fixed_odds_markets$eventName) {
        next
      } else {
        first_basket <-
          fixed_odds_markets$outcomes[fixed_odds_markets$eventName == 'First Basket'][[1]]
      }

      output_list[[length(output_list) + 1]] <- first_basket
    }

    if (prop %in% c('player points alt', 'player points ou', 'player points tiers',
                    'player pts alt', 'player pts ou', 'player pts tiers')) {

      # skip if no first basket props
      if (!'Player Points Markets' %in% fixed_odds_markets$groupName) {
        next
      } else {
        points_markets <-
          fixed_odds_markets[fixed_odds_markets$groupName == 'Player Points Markets', ]
      }

      # skip if no player points props available
      prop <- gsub(' pts ', 'points ', prop)

      # get correct props
      prop_type <- ifelse(grepl('alt$', prop), 'points alt',
                          ifelse(grepl('ou$', prop), 'points ou',
                                 ifelse(grepl('tiers$', prop), 'points tiers',
                                        NA_character_
                                 )))

      # handle the different kinds of player points bets
      if (prop_type == 'points ou') {
        ous <- points_markets[grepl('[A-Z] Points Over/Under', points_markets$eventName), ]
        output_list[[length(output_list) + 1]] <- do.call(rbind, ous$outcomes)
      }

      if (prop_type == 'points tiers') {
        tiers <- points_markets[grepl('Pick', points_markets$eventName), ]
        output_list[[length(output_list) + 1]] <- do.call(rbind, tiers$outcomes)
      }

    }

    if (prop %in% c('player rebounds alt', 'player rebounds ou', 'player rebounds tiers',
                    'player rebs alt', 'player rebs ou', 'player rebs tiers')) {

      # skip if no first basket props
      if (!'Player Rebounds Markets' %in% fixed_odds_markets$groupName) {
        next
      } else {
        rebounds_markets <-
          fixed_odds_markets[fixed_odds_markets$groupName == 'Player Rebounds Markets', ]
      }

      # skip if no player rebounds props available
      prop <- gsub(' rebs ', 'rebounds ', prop)

      # get correct props
      prop_type <- ifelse(grepl('alt$', prop), 'rebounds alt',
                          ifelse(grepl('ou$', prop), 'rebounds ou',
                                 ifelse(grepl('tiers$', prop), 'rebounds tiers',
                                        NA_character_
                                 )))

      # handle the different kinds of player rebounds bets
      if (prop_type == 'rebounds ou') {
        ous <- rebounds_markets[grepl('[A-Z] Rebounds Over/Under', rebounds_markets$eventName), ]
        output_list[[length(output_list) + 1]] <- do.call(rbind, ous$outcomes)
      }

      if (prop_type == 'rebounds tiers') {
        tiers <- rebounds_markets[grepl('Pick', rebounds_markets$eventName), ]
        output_list[[length(output_list) + 1]] <- do.call(rbind, tiers$outcomes)
      }

    }

    if (prop %in% c('player assists alt', 'player assists ou', 'player assists tiers',
                    'player asts alt', 'player asts ou', 'player asts tiers')) {

      # skip if no first basket props
      if (!'Player Assists Markets' %in% fixed_odds_markets$groupName) {
        next
      } else {
        assists_markets <-
          fixed_odds_markets[fixed_odds_markets$groupName == 'Player Assists Markets', ]
      }

      # skip if no player assists props available
      prop <- gsub(' asts ', 'assists ', prop)

      # get correct props
      prop_type <- ifelse(grepl('alt$', prop), 'assists alt',
                          ifelse(grepl('ou$', prop), 'assists ou',
                                 ifelse(grepl('tiers$', prop), 'assists tiers',
                                        NA_character_
                                 )))

      # handle the different kinds of player assists bets
      if (prop_type == 'assists ou') {
        ous <- assists_markets[grepl('[A-Z] Assists Over/Under', assists_markets$eventName), ]
        output_list[[length(output_list) + 1]] <- do.call(rbind, ous$outcomes)
      }

      if (prop_type == 'assists tiers') {
        tiers <- assists_markets[grepl('Pick', assists_markets$eventName), ]
        output_list[[length(output_list) + 1]] <- do.call(rbind, tiers$outcomes)
      }

    }

  }

  # if output_list is empty, error, else return as a data.frame
  if (length(output_list) == 0) {
    stop('no props returned')
  } else {
    output_df <- do.call(rbind, output_list)
  }
  return(output_df)
}
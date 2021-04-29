parse_pointsbet_data <- function(pointsbet_data, prop) {
  # loop through the pointsbet events to extract props
  output_list <- list()
  for (game_event in pointsbet_data) {
    # check for fixed odds markets, skip if they're not there
    if ('fixedOddsMarkets' %in% names(game_event)) fixed_odds_markets <- game_event$fixedOddsMarkets else next
    event_names <- unlist(lapply(fixed_odds_markets, '[[', 'eventName'))
    # now extract correct props
    if (prop %in% c('first team to score', 'ftts')) {
      if ('First Team to Score' %in% event_names) {
        outcomes <- fixed_odds_markets[[which(grepl('^First Team to Score$', event_names))]]$outcomes
        first_team_to_score <- as.data.frame(do.call(rbind, outcomes))
      } else {
        next
      }
      output_list[[length(output_list) + 1]] <- first_team_to_score
    }
    if (prop %in% c('first player to score', 'fpts')) {
      # skip if no first basket props
      if ('First Basket' %in% event_names) {
        outcomes <- fixed_odds_markets[[which(grepl('^First Basket$', event_names))]]$outcomes
        first_basket <- as.data.frame(do.call(rbind, outcomes))
      } else {
        next
      }
      output_list[[length(output_list) + 1]] <- first_basket
    }
    if (prop %in% c('player points alt', 'player points ou', 'player points tiers',
                    'player pts alt', 'player pts ou', 'player pts tiers')) {
      # skip if no player points props available
      prop <- gsub(' pts ', 'points ', prop)
      # get correct props
      prop_type <- ifelse(grepl('alt$', prop), 'points alt',
                          ifelse(grepl('ou$', prop), 'points ou',
                                 ifelse(grepl('tiers$', prop), 'points tiers',
                                        NA_character_
                                 )))
      # handle the different kinds of player points bets
      if (prop_type == 'points alt') {
        if (any(grepl('Pick Your Own Points', event_names))) {
          elements <- which(grepl('Pick Your Own Points', event_names))
          selected_markets <- fixed_odds_markets[elements]
          outcomes <- lapply(selected_markets, '[[', 'outcomes')
          points_alt_list <- list()
          for (o in outcomes) {
            extracted <- as.data.frame(do.call(rbind, o))
            points_alt_list[[length(points_alt_list) + 1]] <- extracted
          }
          points_alt <- do.call(rbind, points_alt_list)
        } else {
          next
        }
        output_list[[length(output_list) + 1]] <- points_alt
      }
      if (prop_type == 'points ou') {
        if (any(grepl('[A-Z] Points Over/Under', event_names))) {
          elements <- which(grepl('[A-Z] Points Over/Under', event_names))
          selected_markets <- fixed_odds_markets[elements]
          outcomes <- lapply(selected_markets, '[[', 'outcomes')
          points_ou_list <- list()
          for (o in outcomes) {
            extracted <- as.data.frame(do.call(rbind, o))
            points_ou_list[[length(points_ou_list) + 1]] <- extracted
          }
          points_ou <- do.call(rbind, points_ou_list)
        } else {
          next
        }
        output_list[[length(output_list) + 1]] <- points_ou
      }
      if (prop_type == 'points tiers') {
        if (any(grepl('Pick Your Own Points', event_names))) {
          elements <- which(grepl('Pick Your Own Points', event_names))
          selected_markets <- fixed_odds_markets[elements]
          outcomes <- lapply(selected_markets, '[[', 'outcomes')
          points_tiers_list <- list()
          for (o in outcomes) {
            extracted <- as.data.frame(do.call(rbind, o))
            points_tiers_list[[length(points_tiers_list) + 1]] <- extracted
          }
          points_tiers <- do.call(rbind, points_tiers_list)
        } else {
          next
        }
        output_list[[length(output_list) + 1]] <- points_tiers
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

    if (prop %in% c('player three-pointers alt', 'player three-pointers ou', 'player three-pointers tiers',
                    'player 3pts alt', 'player 3pts ou', 'player 3pts tiers')) {

      # skip if no 3 pointer props
      if (!'Player Three Pointer Wagers' %in% fixed_odds_markets$groupName) {
        next
      } else {
        threes_markets <-
          fixed_odds_markets[fixed_odds_markets$groupName == 'Player Three Pointer Wagers', ]
      }

      # skip if no player assists props available
      prop <- gsub(' 3pts ', 'three-pointers ', prop)

      # get correct props
      prop_type <- ifelse(grepl('alt$', prop), 'three-pointers alt',
                          ifelse(grepl('ou$', prop), 'three-pointers ou',
                                 ifelse(grepl('tiers$', prop), 'three-pointers tiers',
                                        NA_character_
                                 )))

      # handle the different kinds of player assists bets
      if (prop_type == 'three-pointers ou') {
        ous <- threes_markets[grepl('Threes Over/Under', threes_markets$eventName), ]
        output_list[[length(output_list) + 1]] <- do.call(rbind, ous$outcomes)
      }

      if (prop_type == 'three-pointers tiers') {
        tiers <- threes_markets[grepl('Pick', threes_markets$eventName), ]
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

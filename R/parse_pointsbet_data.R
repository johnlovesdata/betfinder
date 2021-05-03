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
      # skip if no player rebounds props available
      prop <- gsub(' rebs ', 'rebounds ', prop)
      # get correct props
      prop_type <- ifelse(grepl('alt$', prop), 'rebounds alt',
                          ifelse(grepl('ou$', prop), 'rebounds ou',
                                 ifelse(grepl('tiers$', prop), 'rebounds tiers',
                                        NA_character_
                                 )))
      # handle the different kinds of player rebounds bets
      if (prop_type == 'rebounds alt') {
        if (any(grepl('Pick Your Own Rebounds', event_names))) {
          elements <- which(grepl('Pick Your Own Rebounds', event_names))
          selected_markets <- fixed_odds_markets[elements]
          outcomes <- lapply(selected_markets, '[[', 'outcomes')
          rebounds_alt_list <- list()
          for (o in outcomes) {
            extracted <- as.data.frame(do.call(rbind, o))
            rebounds_alt_list[[length(rebounds_alt_list) + 1]] <- extracted
          }
          rebounds_alt <- do.call(rbind, rebounds_alt_list)
        } else {
          next
        }
        output_list[[length(output_list) + 1]] <- rebounds_alt
      }
      if (prop_type == 'rebounds ou') {
        if (any(grepl('[A-Z] Rebounds Over/Under', event_names))) {
          elements <- which(grepl('[A-Z] Rebounds Over/Under', event_names))
          selected_markets <- fixed_odds_markets[elements]
          outcomes <- lapply(selected_markets, '[[', 'outcomes')
          rebounds_ou_list <- list()
          for (o in outcomes) {
            extracted <- as.data.frame(do.call(rbind, o))
            rebounds_ou_list[[length(rebounds_ou_list) + 1]] <- extracted
          }
          rebounds_ou <- do.call(rbind, rebounds_ou_list)
        } else {
          next
        }
        output_list[[length(output_list) + 1]] <- rebounds_ou
      }
      if (prop_type == 'rebounds tiers') {
        if (any(grepl('Pick Your Own Rebounds', event_names))) {
          elements <- which(grepl('Pick Your Own Rebounds', event_names))
          selected_markets <- fixed_odds_markets[elements]
          outcomes <- lapply(selected_markets, '[[', 'outcomes')
          rebounds_tiers_list <- list()
          for (o in outcomes) {
            extracted <- as.data.frame(do.call(rbind, o))
            rebounds_tiers_list[[length(rebounds_tiers_list) + 1]] <- extracted
          }
          rebounds_tiers <- do.call(rbind, rebounds_tiers_list)
        } else {
          next
        }
        output_list[[length(output_list) + 1]] <- rebounds_tiers
      }
    }
    if (prop %in% c('player assists alt', 'player assists ou', 'player assists tiers',
                    'player asts alt', 'player asts ou', 'player asts tiers')) {
      # skip if no player assists props available
      prop <- gsub(' asts ', 'assists ', prop)
      # get correct props
      prop_type <- ifelse(grepl('alt$', prop), 'assists alt',
                          ifelse(grepl('ou$', prop), 'assists ou',
                                 ifelse(grepl('tiers$', prop), 'assists tiers',
                                        NA_character_
                                 )))
      # handle the different kinds of player assists bets
      if (prop_type == 'assists alt') {
        if (any(grepl('Pick Your Own Assists', event_names))) {
          elements <- which(grepl('Pick Your Own Assists', event_names))
          selected_markets <- fixed_odds_markets[elements]
          outcomes <- lapply(selected_markets, '[[', 'outcomes')
          assists_alt_list <- list()
          for (o in outcomes) {
            extracted <- as.data.frame(do.call(rbind, o))
            assists_alt_list[[length(assists_alt_list) + 1]] <- extracted
          }
          assists_alt <- do.call(rbind, assists_alt_list)
        } else {
          next
        }
        output_list[[length(output_list) + 1]] <- assists_alt
      }
      if (prop_type == 'assists ou') {
        if (any(grepl('[A-Z] Assists Over/Under', event_names))) {
          elements <- which(grepl('[A-Z] Assists Over/Under', event_names))
          selected_markets <- fixed_odds_markets[elements]
          outcomes <- lapply(selected_markets, '[[', 'outcomes')
          assists_ou_list <- list()
          for (o in outcomes) {
            extracted <- as.data.frame(do.call(rbind, o))
            assists_ou_list[[length(assists_ou_list) + 1]] <- extracted
          }
          assists_ou <- do.call(rbind, assists_ou_list)
        } else {
          next
        }
        output_list[[length(output_list) + 1]] <- assists_ou
      }
      if (prop_type == 'assists tiers') {
        if (any(grepl('Pick Your Own Assists', event_names))) {
          elements <- which(grepl('Pick Your Own Assists', event_names))
          selected_markets <- fixed_odds_markets[elements]
          outcomes <- lapply(selected_markets, '[[', 'outcomes')
          assists_tiers_list <- list()
          for (o in outcomes) {
            extracted <- as.data.frame(do.call(rbind, o))
            assists_tiers_list[[length(assists_tiers_list) + 1]] <- extracted
          }
          assists_tiers <- do.call(rbind, assists_tiers_list)
        } else {
          next
        }
        output_list[[length(output_list) + 1]] <- assists_tiers
      }
    }
    if (prop %in% c('player three-pointers alt', 'player three-pointers ou', 'player three-pointers tiers',
                    'player 3pts alt', 'player 3pts ou', 'player 3pts tiers')) {
      # get correct props
      prop_type <- ifelse(grepl('alt$', prop), 'threes alt',
                          ifelse(grepl('ou$', prop), 'threes ou',
                                 ifelse(grepl('tiers$', prop), 'threes tiers',
                                        NA_character_
                                 )))
      # handle the different kinds of player threes bets
      if (prop_type == 'threes alt') {
        if (any(grepl('Pick Your Own Made Threes', event_names))) {
          elements <- which(grepl('Pick Your Own Made Threes', event_names))
          selected_markets <- fixed_odds_markets[elements]
          outcomes <- lapply(selected_markets, '[[', 'outcomes')
          threes_alt_list <- list()
          for (o in outcomes) {
            extracted <- as.data.frame(do.call(rbind, o))
            threes_alt_list[[length(threes_alt_list) + 1]] <- extracted
          }
          threes_alt <- do.call(rbind, threes_alt_list)
        } else {
          next
        }
        output_list[[length(output_list) + 1]] <- threes_alt
      }
      if (prop_type == 'threes ou') {
        if (any(grepl('Threes Over/Under', event_names))) {
          elements <- which(grepl('Threes Over/Under', event_names))
          selected_markets <- fixed_odds_markets[elements]
          outcomes <- lapply(selected_markets, '[[', 'outcomes')
          threes_ou_list <- list()
          for (o in outcomes) {
            extracted <- as.data.frame(do.call(rbind, o))
            threes_ou_list[[length(threes_ou_list) + 1]] <- extracted
          }
          threes_ou <- do.call(rbind, threes_ou_list)
        } else {
          next
        }
        output_list[[length(output_list) + 1]] <- threes_ou
      }
      if (prop_type == 'threes tiers') {
        # noping this out cuz it's so far the same exact thing as the alt pieces
        if (any(grepl('NOOOOOPE', event_names))) {
          elements <- which(grepl('Pick Your Own Threes', event_names))
          selected_markets <- fixed_odds_markets[elements]
          outcomes <- lapply(selected_markets, '[[', 'outcomes')
          threes_tiers_list <- list()
          for (o in outcomes) {
            extracted <- as.data.frame(do.call(rbind, o))
            threes_tiers_list[[length(threes_tiers_list) + 1]] <- extracted
          }
          threes_tiers <- do.call(rbind, threes_tiers_list)
        } else {
          next
        }
        output_list[[length(output_list) + 1]] <- threes_tiers
      }
    }
  }
  # if output_list is empty, error, else return as a data.frame
  if (!'output_list' %in% ls()) stop('no pointsbet ', prop, ' props returned')
  if (length(output_list) == 0) stop('no pointsbet ', prop, ' props returned')
  output_df <- do.call(rbind, output_list)
  return(output_df)
}

parse_fanduel_data <- function(fanduel_data, prop) {

  # loop through fanduel_data and extract the correct prop
  output_list <- list()
  for (game_event in fanduel_data) {

    # pull out event market groups regardless of prop
    if (!'eventmarketgroups' %in% names(game_event)) {
      next
    } else {
      game_event_market_groups <- game_event$eventmarketgroups
    }

    # extract correct props
    if (prop %in% c('first team to score', 'ftts')) {
      # get all props if they exist, else skip
      if (!'All' %in% game_event_market_groups$name) {
        next
      } else {
        all_props <-
          game_event_market_groups$markets[game_event_market_groups$name == 'All'][[1]]
      }

      # if there are no ftts props, skip
      if (!'Team to Score First' %in% all_props$name) {
        next
      } else {
        first_team_to_score <-
          all_props$selections[all_props$name == 'Team to Score First'][[1]]
      }

      # extract description which has the matchup, and stash in output_list
      first_team_to_score$description <- game_event$externaldescription
      output_list[[length(output_list) + 1]] <- first_team_to_score
    }

    if (prop %in% c('first player to score', 'fpts')) {

      # skip if no player props available
      market_label <- 'All Player Props'
      if (!market_label %in% unique(game_event_market_groups$name)) {
        next
      } else {
        player_props <-
          game_event_market_groups$markets[game_event_market_groups$name == market_label][[1]]
      }

      # skip if no first basket
      prop_label <- 'First Basket'
      if (!prop_label %in% player_props$name) {
        next
      } else {
        first_basket <-
          player_props$selections[player_props$name == prop_label][[1]]
      }

      # extract the description which has the matchup, and stash in the output_list
      first_basket$description <- game_event$externaldescription
      output_list[[length(output_list) + 1]] <- first_basket
    }

    if (prop %in% c('player points alt', 'player points ou', 'player points tiers',
                    'player pts alt', 'player pts ou', 'player pts tiers')) {

      # skip if no player points props available
      prop <- gsub(' pts ', 'points ', prop)
      market_label <- 'Player Points'
      if (!market_label %in% unique(game_event_market_groups$name)) {
        next
      } else {
        player_points <-
          game_event_market_groups$markets[game_event_market_groups$name == market_label][[1]]
      }

      # get correct props
      prop_type <- ifelse(grepl('alt', prop), 'points alt',
                   ifelse(grepl('ou', prop), 'points ou',
                   ifelse(grepl('tiers', prop), 'points tiers',
                          NA_character_
                   )))

      # handle the different kinds of player points bets
      if (prop_type == 'points alt') {

        if (length(player_points$name[grepl('Alt', player_points$name)]) < 1) {
          next
          } else {
          alt_props <- player_points[grepl('Alt', player_points$name), ]
          points_df <- do.call(rbind, alt_props$selections)
          }
        }

      if (prop_type == 'points ou') {
          if (length(player_points$name[grepl('- Points', player_points$name)]) < 1) {
            next
          } else {
            ou_props <- player_points[grepl('- Points', player_points$name), ]
            points_df <- do.call(rbind, ou_props$selections)
          }
      }

      if (prop_type == 'points tiers') {
        if (length(player_points$name[grepl('To Score', player_points$name)]) < 1) {
          next
        } else {
          thresh_props <- player_points[grepl('To Score', player_points$name), ]
        }
        # gotta label the points values somehow, which is to use the name of the field, requiring a loop i think
        points_list <- list()
        for (n  in 1:nrow(thresh_props)) {
          selections <- thresh_props$selections[[n]]
          if (nrow(selections) < 1) {
            next
          }  else {
            selections$prop_details <- thresh_props$name[[n]]
            points_list[[length(points_list) + 1]] <- selections
          }
        }
        points_df <- do.call(rbind, points_list)
      }

      # extract the description, which has the matchup, and stash in the output_list
      points_df$description <- game_event$externaldescription
      output_list[[length(output_list) + 1]] <- points_df
    }

    if (prop %in% c('player rebounds alt', 'player rebounds ou', 'player rebounds tiers',
                    'player rebs alt', 'player rebs ou', 'player rebs tiers')) {

      # skip if no player rebounds props available
      prop <- gsub(' rebs ', ' rebounds ', prop)
      market_label <- 'Player Rebounds'
      if (!market_label %in% unique(game_event_market_groups$name)) {
        next
      } else {
        player_rebounds <-
          game_event_market_groups$markets[game_event_market_groups$name == market_label][[1]]
      }

      # get correct props
      prop_type <- ifelse(grepl('alt$', prop), 'rebounds alt',
                   ifelse(grepl('ou$', prop), 'rebounds ou',
                   ifelse(grepl('tiers$', prop), 'rebounds tiers',
                          NA_character_
                                 )))

      # handle the different kinds of player rebounds bets
      if (prop_type == 'rebounds alt') {

        if (length(player_rebounds$name[grepl('Alt', player_rebounds$name)]) < 1) {
          next
        } else {
          alt_props <- player_rebounds[grepl('Alt', player_rebounds$name), ]
          rebounds_df <- do.call(rbind, alt_props$selections)
        }
      }

      if (prop_type == 'rebounds ou') {
        if (length(player_rebounds$name[grepl('- Rebounds', player_rebounds$name)]) < 1) {
          next
        } else {
          ou_props <- player_rebounds[grepl('- Rebounds', player_rebounds$name), ]
          rebounds_df <- do.call(rbind, ou_props$selections)
        }
      }

      if (prop_type == 'rebounds tiers') {
        if (length(player_rebounds$name[grepl('^To Record*.*Rebounds$', player_rebounds$name)]) < 1) {
          next
        } else {
          thresh_props <- player_rebounds[grepl('^To Record*.*Rebounds$', player_rebounds$name), ]
        }
        # gotta label the rebounds values somehow, which is to use the name of the field, requiring a loop i think
        rebounds_list <- list()
        for (n  in 1:nrow(thresh_props)) {
          selections <- thresh_props$selections[[n]]
          if (nrow(selections) < 1) {
            next
          }  else {
            selections$prop_details <- thresh_props$name[[n]]
            rebounds_list[[length(rebounds_list) + 1]] <- selections
          }
        }
        rebounds_df <- do.call(rbind, rebounds_list)
      }

      # extract the description, which has the matchup, and stash in the output_list
      rebounds_df$description <- game_event$externaldescription
      output_list[[length(output_list) + 1]] <- rebounds_df
    }

    if (prop %in% c('player assists alt', 'player assists ou', 'player assists tiers',
                    'player asts alt', 'player asts ou', 'player asts tiers')) {

      # skip if no player assists props available
      prop <- gsub(' asts ', ' assists ', prop)
      market_label <- 'Player Assists'
      if (!market_label %in% unique(game_event_market_groups$name)) {
        next
      } else {
        player_assists <-
          game_event_market_groups$markets[game_event_market_groups$name == market_label][[1]]
      }

      # get correct props
      prop_type <- ifelse(grepl('alt$', prop), 'assists alt',
                   ifelse(grepl('ou$', prop), 'assists ou',
                   ifelse(grepl('tiers$', prop), 'assists tiers',
                          NA_character_
                                 )))

      # handle the different kinds of player assists bets
      if (prop_type == 'assists alt') {

        if (length(player_assists$name[grepl('Alt', player_assists$name)]) < 1) {
          next
        } else {
          alt_props <- player_assists[grepl('Alt', player_assists$name), ]
          assists_df <- do.call(rbind, alt_props$selections)
        }
      }

      if (prop_type == 'assists ou') {
        if (length(player_assists$name[grepl('- Assists', player_assists$name)]) < 1) {
          next
        } else {
          ou_props <- player_assists[grepl('- Assists', player_assists$name), ]
          assists_df <- do.call(rbind, ou_props$selections)
        }
      }

      if (prop_type == 'assists tiers') {
        if (length(player_assists$name[grepl('^To Record*.*Assists$', player_assists$name)]) < 1) {
          next
        } else {
          thresh_props <- player_assists[grepl('^To Record*.*Assists$', player_assists$name), ]
        }
        # gotta label the assists values somehow, which is to use the name of the field, requiring a loop i think
        assists_list <- list()
        for (n  in 1:nrow(thresh_props)) {
          selections <- thresh_props$selections[[n]]
          if (nrow(selections) < 1) {
            next
          }  else {
            selections$prop_details <- thresh_props$name[[n]]
            assists_list[[length(assists_list) + 1]] <- selections
          }
        }
        assists_df <- do.call(rbind, assists_list)
      }

      # extract the description, which has the matchup, and stash in the output_list
      assists_df$description <- game_event$externaldescription
      output_list[[length(output_list) + 1]] <- assists_df
    }

    if (prop %in% c('player three-pointers alt', 'player three-pointers ou', 'player three-pointers tiers',
                    'player 3pts alt', 'player 3pts ou', 'player 3pts tiers')) {

      # skip if no player threes props available
      prop <- gsub(' 3pts ', ' three-pointers ', prop)
      market_label <- 'Player Threes'
      if (!market_label %in% unique(game_event_market_groups$name)) {
        next
      } else {
        player_threes <-
          game_event_market_groups$markets[game_event_market_groups$name == market_label][[1]]
      }

      # get correct props
      prop_type <- ifelse(grepl('alt$', prop), 'three-pointers alt',
                          ifelse(grepl('ou$', prop), 'three-pointers ou',
                                 ifelse(grepl('tiers$', prop), 'three-pointers tiers',
                                        NA_character_
                                 )))

      # handle the different kinds of player threes bets
      if (prop_type == 'three-pointers alt') {

        if (length(player_threes$name[grepl('Alt', player_threes$name)]) < 1) {
          next
        } else {
          alt_props <- player_threes[grepl('Alt', player_threes$name), ]
          if (nrow(alt_props) < 1) {
            next
          } else {
            threes_df <- do.call(rbind, alt_props$selections)
          }

        }
      }

      if (prop_type == 'three-pointers ou') {
        if (length(player_threes$name[grepl('- Made Threes', player_threes$name)]) < 1) {
          next
        } else {
          ou_props <- player_threes[grepl('- Made Threes', player_threes$name), ]
          threes_df <- do.call(rbind, ou_props$selections)
        }
      }

      if (prop_type == 'three-pointers tiers') {
        if (length(player_threes$name[grepl('\\+ Made Threes', player_threes$name)]) < 1) {
          next
        } else {
          thresh_props <- player_threes[grepl('\\+ Made Threes', player_threes$name), ]
        }
        # gotta label the threes values somehow, which is to use the name of the field, requiring a loop i think
        threes_list <- list()
        for (n  in 1:nrow(thresh_props)) {
          selections <- thresh_props$selections[[n]]
          if (nrow(selections) < 1) {
            next
          }  else {
            selections$prop_details <- thresh_props$name[[n]]
            threes_list[[length(threes_list) + 1]] <- selections
          }
        }
        threes_df <- do.call(rbind, threes_list)
      }

      # extract the description, which has the matchup, and stash in the output_list
      threes_df$description <- game_event$externaldescription
      output_list[[length(output_list) + 1]] <- threes_df
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

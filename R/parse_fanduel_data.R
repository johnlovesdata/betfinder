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

    if (prop %in% c('points_player_alt', 'points_player_ou', 'points_threshold',
                    'pts_player_alt', 'pts_player_ou', 'pts_threshold')) {

      # skip if no player points props available
      market_label <- 'Player Points'
      if (!market_label %in% unique(game_event_market_groups$name)) {
        next
      } else {
        player_points <-
          game_event_market_groups$markets[game_event_market_groups$name == market_label][[1]]
      }

      # get correct props
      prop_type <- ifelse(grepl('_alt', prop), 'alt',
                   ifelse(grepl('_ou', prop), 'ou',
                   ifelse(grepl('_threshold', prop), 'threshold', NA_character_
                   )))

      # handle the different kinds of player points bets
      if (prop_type == 'alt') {

        # if there aren't any alt props yet, move on
        if (length(player_points$name[grepl('alt', player_points$name)]) < 1) {
          next
          } else {
          points_df <- data.frame()
          }
        }

      if (prop_type == 'ou') {
          if (length(player_points$name[grepl('- Points', player_points$name)]) < 1) {
            next
          } else {
            ou_props <- player_points[grepl('- Points', player_points$name), ]
            points_df <- do.call(rbind, ou_props$selections)
          }
      }

      if (prop_type == 'threshold') {
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
            selections$prop <- thresh_props$name[[n]]
            points_list[[length(points_list) + 1]] <- selections
          }
        }
        points_df <- do.call(rbind, points_list)
      }

      # extract the description, which has the matchup, and stash in the output_list
      points_df$description <- game_event$externaldescription
      output_list[[length(output_list) + 1]] <- points_df
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

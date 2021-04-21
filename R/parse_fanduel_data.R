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
      if (!'All Player Props' %in% unique(game_event_market_groups$name)) {
        next
      } else {
        player_props <-
          game_event_market_groups$markets[game_event_market_groups$name == 'All Player Props'][[1]]
      }

      # skip if no first basket
      if (!'First Basket' %in% player_props$name) {
        next
      } else {
        first_basket <-
          player_props$selections[player_props$name == 'First Basket'][[1]]
      }

      # extract the description which has the matchup, and stash in the output_list
      first_basket$description <- game_event$externaldescription
      output_list[[length(output_list) + 1]] <- first_basket
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


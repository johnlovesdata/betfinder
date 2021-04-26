parse_draftkings_data <- function(draftkings_data, prop) {

  # break out the offer markets
  # TODO: MAKE SURE THIS WORKS WITH ALL SPORTS ASSHOLE; it probably won't for like golf?
  offer_categories <- draftkings_data$eventGroup$offerCategories

  if (prop %in% c('first team to score', 'ftts')) {
    # if there are no game props, error
    if (!('Game Props' %in% offer_categories$name)) {
      stop('no Game Props available')
    } else {
      game_props <-
        offer_categories$offerSubcategoryDescriptors[offer_categories$name == "Game Props"][[1]]
    }

    # if there are no ftts props, error
    if (!'First Team to Score' %in% game_props$offerSubcategory$name) {
      stop('no First Team to Score available')
    } else {
      first_team_to_score <-
        game_props[game_props$offerSubcategory == "First Team to Score",]$offerSubcategory$offers[[1]]
      outcomes <- do.call(rbind, first_team_to_score)$outcomes
    }

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
  }

  if (prop %in% c('first player to score', 'fpts')) {

    # error if no player props
    if (!'Player Props' %in% offer_categories$name) {
      stop('no Player Props available')
    } else {
      player_props <-
        offer_categories[offer_categories$name == 'Player Props',]$offerSubcategoryDescriptors[[1]]$offerSubcategory
    }

    # error if no fpts
    if (!'First Field Goal' %in% player_props$name) {
      stop('no First Field Goal props available')
    } else {
      ffg <-
        player_props$offers[player_props$name == 'First Field Goal'][[1]]
    }

    # loop through the outcomes to get all the fpts props
    output_list <- list()
    for (i in 1:length(ffg)) {
      outcomes <- ffg[[i]]$outcomes[[1]]
      output_list[[i]] <- outcomes
    }
  }

  if (prop %in% c('player points ou', 'player pts ou')) {

    # error if no player props
    if (!'Player Props' %in% offer_categories$name) {
      stop('no Player Props available')
    } else {
      player_props <-
        offer_categories[offer_categories$name == 'Player Props',]$offerSubcategoryDescriptors[[1]]$offerSubcategory
    }

    # error if no Points props
    if (!'Points' %in% player_props$name) {
      stop('no Points props available')
    } else {
      pp <-
        player_props$offers[player_props$name == 'Points'][[1]]
    }
    # all of the player props are in these outcomes objects, so grab em all
    output_list <- list()
    for (p in seq_along(pp)) {
      outcomes <- pp[[p]]$outcomes
      binded_outcomes <- do.call(rbind, outcomes)
      if (!'participant' %in% names(binded_outcomes)) {
        next
      }
      output_list[[length(output_list) + 1]] <- binded_outcomes
    }
  }

  if (prop %in% c('player rebounds ou', 'player rebs ou')) {

    # error if no player props
    if (!'Player Props' %in% offer_categories$name) {
      stop('no Player Props available')
    } else {
      player_props <-
        offer_categories[offer_categories$name == 'Player Props',]$offerSubcategoryDescriptors[[1]]$offerSubcategory
    }

    # error if no Rebounds props
    if (!'Rebounds' %in% player_props$name) {
      stop('no Rebounds props available')
    } else {
      pp <-
        player_props$offers[player_props$name == 'Rebounds'][[1]]
    }
    # all of the player props are in these outcomes objects, so grab em all
    output_list <- list()
    for (p in seq_along(pp)) {
      outcomes <- pp[[p]]$outcomes
      output_list[[length(output_list) + 1]] <- do.call(rbind, outcomes)
    }
  }

  if (prop %in% c('player assists ou', 'player asts ou')) {

    # error if no player props
    if (!'Player Props' %in% offer_categories$name) {
      stop('no Player Props available')
    } else {
      player_props <-
        offer_categories[offer_categories$name == 'Player Props',]$offerSubcategoryDescriptors[[1]]$offerSubcategory
    }

    # error if no Assists props
    if (!'Assists' %in% player_props$name) {
      stop('no Assists props available')
    } else {
      pp <-
        player_props$offers[player_props$name == 'Assists'][[1]]
    }
    # all of the player props are in these outcomes objects, so grab em all
    output_list <- list()
    for (p in seq_along(pp)) {
      outcomes <- pp[[p]]$outcomes
      output_list[[length(output_list) + 1]] <- do.call(rbind, outcomes)
    }
  }

  if (prop %in% c('player three-pointers ou', 'player 3pts ou')) {

    # error if no player props
    if (!'Player Props' %in% offer_categories$name) {
      stop('no Player Props available')
    } else {
      player_props <-
        offer_categories[offer_categories$name == 'Player Props',]$offerSubcategoryDescriptors[[1]]$offerSubcategory
    }

    # error if no Assists props
    if (!'3-Pointers' %in% player_props$name) {
      stop('no 3-Pointers props available')
    } else {
      pp <-
        player_props$offers[player_props$name == '3-Pointers'][[1]]
    }
    # all of the player props are in these outcomes objects, so grab em all
    output_list <- list()
    for (p in seq_along(pp)) {
      outcomes <- pp[[p]]$outcomes
      output_list[[length(output_list) + 1]] <- do.call(rbind, outcomes)
    }
  }

  # if output_list is empty, error
  if (length(output_list) == 0) {
    stop('no props returned')
  } else {
    output_df <- do.call(rbind, output_list)
  }
  return(output_df)
}




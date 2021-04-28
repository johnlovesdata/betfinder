parse_draftkings_data <- function(draftkings_data, prop) {

  # break out the offer markets, always necessary
  offer_categories <- draftkings_data$eventGroup$offerCategories
  offer_category_names <- unlist(lapply(offer_categories, '[[', 'name'))

  # extract the prop
  if (prop %in% c('first team to score', 'ftts')) {
    # if there are no game props, error
    if (!('Game Props' %in% offer_category_names)) stop('no Game Props available')
    game_props <- offer_categories[[which(offer_category_names == 'Game Props')]]$offerSubcategoryDescriptors
    game_prop_names <- unlist(lapply(game_props, '[[', 'name'))
    # if there are no ftts props, error
    if (!'First Team to Score' %in% game_prop_names) stop('no First Team to Score available')
    first_team_to_score <- game_props[[which(game_prop_names == 'First Team to Score')]]$offerSubcategory$offers
    output_list <- list()
    for (ftts in first_team_to_score) {
      outcomes <- ftts[[1]]$outcomes
      output_list[[length(output_list) + 1]] <- as.data.frame(do.call(rbind, outcomes))
    }
  }
  if (prop %in% c('first player to score', 'fpts')) {
    # if there are no game props, error
    if (!('Player Props' %in% offer_category_names)) stop('no Player Props available')
    player_props <- offer_categories[[which(offer_category_names == 'Player Props')]]$offerSubcategoryDescriptors
    player_prop_names <- unlist(lapply(player_props, '[[', 'name'))
    # if there are no ftts props, error
    if (!'First Field Goal' %in% player_prop_names) stop('no First Field Goal available')
    first_player_to_score <- player_props[[which(player_prop_names == 'First Field Goal')]]$offerSubcategory$offers
    output_list <- list()
    for (fpts in first_player_to_score) {
      outcomes <- fpts[[1]]$outcomes
      output_list[[length(output_list) + 1]] <- as.data.frame(do.call(rbind, outcomes))
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
    output_df <- as.data.frame(do.call(rbind, output_list))
  }
  return(output_df)
}




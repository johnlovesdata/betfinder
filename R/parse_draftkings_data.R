parse_draftkings_data <- function(draftkings_data, prop) {
  # break out the offer markets, always necessary
  offer_categories <- draftkings_data$eventGroup$offerCategories
  offer_category_names <- unlist(lapply(offer_categories, '[[', 'name'))
  # extract the prop type based on the prop name
  if (grepl('team|ftts', prop)) {
    if (!('Game Props' %in% offer_category_names)) stop('no draftkings ', prop, ' available')
    game_props <- offer_categories[[which(offer_category_names == 'Game Props')]]$offerSubcategoryDescriptors
    game_prop_names <- unlist(lapply(game_props, '[[', 'name'))
  }
  if (grepl('player|fpts', prop)) {
    if (!('Player Props' %in% offer_category_names)) stop('no draftkings ', prop, ' available')
    player_props <- offer_categories[[which(offer_category_names == 'Player Props')]]$offerSubcategoryDescriptors
    player_prop_names <- unlist(lapply(player_props, '[[', 'name'))
  }
  # get the specific props
  if (prop %in% c('first team to score', 'ftts')) {
    if (!'First Team to Score' %in% game_prop_names) stop('no draftkings ', prop, ' available')
    first_team_to_score <- game_props[[which(game_prop_names == 'First Team to Score')]]$offerSubcategory$offers
    output_list <- list()
    for (ftts in first_team_to_score) {
      outcomes <- ftts[[1]]$outcomes
      output_list[[length(output_list) + 1]] <- as.data.frame(do.call(rbind, outcomes))
    }
  }
  if (prop %in% c('first player to score', 'fpts')) {
    # if there are no ftts props, error
    if (!'First Field Goal' %in% player_prop_names) stop('no draftkings ', prop, ' available')
    first_player_to_score <- player_props[[which(player_prop_names == 'First Field Goal')]]$offerSubcategory$offers
    output_list <- list()
    for (fpts in first_player_to_score) {
      outcomes <- fpts[[1]]$outcomes
      dtfrm <- as.data.frame(do.call(rbind, outcomes))
      if ('hidden' %in% names(dtfrm)) dtfrm$hidden <- NULL
      output_list[[length(output_list) + 1]] <- dtfrm
    }
  }
  if (prop %in% c('player points ou', 'player pts ou')) {
    if (!'Points' %in% player_prop_names) stop('no draftkings ', prop, ' available')
    points_props <- player_props[[which(player_prop_names == 'Points')]]$offerSubcategory$offers
    output_list <- list()
    for (ppts in points_props) {
      outcomes <- ppts[[1]]$outcomes
      output_list[[length(output_list) + 1]] <- as.data.frame(do.call(rbind, outcomes))
    }
  }
  if (prop %in% c('player assists ou', 'player asts ou')) {
    # error if no player props
    if (!('Player Props' %in% offer_category_names)) stop('no draftkings ', prop, ' available')
    player_props <- offer_categories[[which(offer_category_names == 'Player Props')]]$offerSubcategoryDescriptors
    player_prop_names <- unlist(lapply(player_props, '[[', 'name'))
    assists_props <- player_props[[which(player_prop_names == 'Assists')]]$offerSubcategory$offers
    output_list <- list()
    for (ppts in assists_props) {
      outcomes <- ppts[[1]]$outcomes
      output_list[[length(output_list) + 1]] <- as.data.frame(do.call(rbind, outcomes))
    }
  }
  if (prop %in% c('player rebounds ou', 'player rebs ou')) {
    # error if no player props
    if (!('Player Props' %in% offer_category_names)) stop('no draftkings ', prop, ' available')
    player_props <- offer_categories[[which(offer_category_names == 'Player Props')]]$offerSubcategoryDescriptors
    player_prop_names <- unlist(lapply(player_props, '[[', 'name'))
    rebounds_props <- player_props[[which(player_prop_names == 'Rebounds')]]$offerSubcategory$offers
    output_list <- list()
    for (ppts in rebounds_props) {
      outcomes <- ppts[[1]]$outcomes
      output_list[[length(output_list) + 1]] <- as.data.frame(do.call(rbind, outcomes))
    }
  }
  if (prop %in% c('player three-pointers ou', 'player 3pts ou')) {
    # error if no player props
    if (!('Player Props' %in% offer_category_names)) stop('no draftkings ', prop, ' available')
    player_props <- offer_categories[[which(offer_category_names == 'Player Props')]]$offerSubcategoryDescriptors
    player_prop_names <- unlist(lapply(player_props, '[[', 'name'))
    threes_props <- player_props[[which(player_prop_names == '3-Pointers')]]$offerSubcategory$offers
    output_list <- list()
    for (ppts in threes_props) {
      outcomes <- ppts[[1]]$outcomes
      output_list[[length(output_list) + 1]] <- as.data.frame(do.call(rbind, outcomes))
    }
  }

  # if output_list is empty, error

  if (!'output_list' %in% ls()) stop('no draftkings ', prop, ' returned')
  if (length(output_list) == 0) stop('no draftkings ', prop, ' returned')
  output_df <- as.data.frame(do.call(rbind, output_list))
  output_df$prop <- prop
  return(output_df)
}




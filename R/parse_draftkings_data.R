parse_draftkings_data <- function(draftkings_data, prop) {

  # get the events (to tie bets with games)
  events <- suppressWarnings(as.data.frame(do.call(rbind, draftkings_data$eventGroup$events)))
  events <- as.data.frame(dplyr::select(events, dplyr::ends_with('Id'), startDate, name))
  events <- as.data.frame(apply(events, 2, unlist))
  events <- events[grepl('@', events$name), ]
  events <- events[!grepl('/', events$name), ]

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
    output_list <- parse_dk_prop(game_props, 'First Team to Score', prop, game_prop_names)
  }
  if (prop %in% c('first player to score', 'fpts')) {
    output_list <- parse_dk_prop(player_props, 'First Field Goal', prop, player_prop_names)
  }
  if (prop %in% c('player points ou', 'player pts ou')) {
    output_list <- parse_dk_prop(player_props, 'Points', prop, player_prop_names)
  }
  if (prop %in% c('player assists ou', 'player asts ou')) {
    output_list <- parse_dk_prop(player_props, 'Assists', prop, player_prop_names)
  }
  if (prop %in% c('player rebounds ou', 'player rebs ou')) {
    output_list <- parse_dk_prop(player_props, 'Rebounds', prop, player_prop_names)
  }
  if (prop %in% c('player three-pointers ou', 'player 3pts ou')) {
    output_list <- parse_dk_prop(player_props, '3-Pointers', prop, player_prop_names)
  }
  if (prop %in% c('player most points')) {
    output_list <- parse_dk_prop(player_props, 'Top Point Scorer', prop, player_prop_names)
  }
  if (prop %in% c('player double double')) {
    output_list <- parse_dk_prop(player_props, 'Double-Double', prop, player_prop_names)
  }
  if (prop %in% c('player triple double')) {
    output_list <- parse_dk_prop(player_props, 'Triple-Double', prop, player_prop_names)
  }
  if (prop %in% c('player strikeouts ou')) {
    output_list <- parse_dk_prop(player_props, 'Strikeouts by Pitcher', prop, player_prop_names)
  }
  if (prop %in% c('player hits ou')) {
    output_list <- parse_dk_prop(player_props, 'Total Hits', prop, player_prop_names)
  }
  if (prop %in% c('player rbis ou')) {
    output_list <- parse_dk_prop(player_props, 'Total Runs Batted In', prop, player_prop_names)
  }
  if (prop %in% c('player runs ou')) {
    output_list <- parse_dk_prop(player_props, 'Total Runs Scored', prop, player_prop_names)
  }
  # TODO: sort out details cuz this is a weird one, cuz i'm sure we'll be able to get tiers on >1 HR
  if (prop %in% c('player to hit home run', 'player to hit hr')) {
    output_list <- parse_dk_prop(player_props, 'Player to Hit Home Run', prop, player_prop_names)
  }

  # if output_list is empty, error
  if (!'output_list' %in% ls()) stop('no draftkings ', prop, ' returned')
  if (length(output_list) == 0) stop('no draftkings ', prop, ' returned')
  output_df <- as.data.frame(apply(do.call(rbind, output_list), 2, unlist))
  output_df$prop <- prop

  # add the events
  output_df <- merge(output_df, events)

  return(output_df)
  }




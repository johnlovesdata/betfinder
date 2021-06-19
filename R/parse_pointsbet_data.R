parse_pointsbet_data <- function(pointsbet_data, prop) {
  # loop through the pointsbet events to extract props
  output_list <- list()
  for (game_event in pointsbet_data) {
    # check for fixed odds markets, skip if they're not there
    if (!'fixedOddsMarkets' %in% names(game_event)) next
    fixed_odds_markets <- game_event$fixedOddsMarkets
    event_names <- unlist(lapply(fixed_odds_markets, '[[', 'eventName'))
    # now extract correct props
    if (prop %in% c('first team to score', 'ftts')) {
      output_list[[length(output_list) + 1]] <-
        parse_pb_prop(game_event = game_event, fixed_odds_markets = fixed_odds_markets, event_names = event_names,
                       prop_regex = 'First Team to Score')
    }
    if (prop %in% c('first player to score', 'fpts')) {
      output_list[[length(output_list) + 1]] <-
        parse_pb_prop(game_event = game_event, fixed_odds_markets = fixed_odds_markets, event_names = event_names,
                       prop_regex = '^First Basket$')
    }
    if (prop %in% c('player pts alt', 'player points alt')) {
      output_list[[length(output_list) + 1]] <-
        parse_pb_prop(game_event = game_event, fixed_odds_markets = fixed_odds_markets, event_names = event_names,
                       prop_regex = 'Pick Your Own Points')
    }
    if (prop %in% c('player pts ou', 'player points ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_pb_prop(game_event = game_event, fixed_odds_markets = fixed_odds_markets, event_names = event_names,
                       prop_regex = '[a-z] Points Over/Under$')
    }
    if (prop %in% c('player rebs alt', 'player rebounds alt')) {
      output_list[[length(output_list) + 1]] <-
        parse_pb_prop(game_event = game_event, fixed_odds_markets = fixed_odds_markets, event_names = event_names,
                       prop_regex = 'Pick Your Own Rebounds')
    }
    if (prop %in% c('player rebs ou', 'player rebounds ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_pb_prop(game_event = game_event, fixed_odds_markets = fixed_odds_markets, event_names = event_names,
                       prop_regex = '[a-z] Rebounds Over/Under$')
    }
    if (prop %in% c('player asts alt', 'player assists alt')) {
      output_list[[length(output_list) + 1]] <-
        parse_pb_prop(game_event = game_event, fixed_odds_markets = fixed_odds_markets, event_names = event_names,
                       prop_regex = 'Pick Your Own Assists')
    }
    if (prop %in% c('player asts ou', 'player assists ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_pb_prop(game_event = game_event, fixed_odds_markets = fixed_odds_markets, event_names = event_names,
                       prop_regex = '[a-z] Assists Over/Under$')
    }
    if (prop %in% c('player 3pts alt', 'player three-pointers alt')) {
      output_list[[length(output_list) + 1]] <-
        parse_pb_prop(game_event = game_event, fixed_odds_markets = fixed_odds_markets, event_names = event_names,
                       prop_regex = 'Pick Your Own Made Threes')
    }
    if (prop %in% c('player 3pts ou', 'player three-pointers ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_pb_prop(game_event = game_event, fixed_odds_markets = fixed_odds_markets, event_names = event_names,
                       prop_regex = '[a-z] Threes Over/Under$')
    }
  }
  # if output_list is empty, error, else return as a data.frame
  if (!'output_list' %in% ls()) stop('no pointsbet ', prop, ' props returned')
  if (length(output_list) == 0) stop('no pointsbet ', prop, ' props returned')

  output_df <- dplyr::bind_rows(output_list)
  return(output_df)
}

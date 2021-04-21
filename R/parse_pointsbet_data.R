parse_pointsbet_data <- function(pointsbet_data, prop) {

  # loop through the pointsbet events to extract props
  output_list <- list()
  for (game_event in pointsbet_data) {

    # check for fixed odds markets, skip if they're not there
    if (!'fixedOddsMarkets' %in% names(game_event)) {
      next
    } else {
      fixed_odds_markets <- game_event$fixedOddsMarkets
    }

    # now extract correct props
    if (prop %in% c('first team to score', 'ftts')) {

      # skip if there's no ftts
      if (!'First Team to Score' %in% fixed_odds_markets$eventName) {
        next
        } else {
          first_team_to_score <-
            fixed_odds_markets$outcomes[fixed_odds_markets$eventName == 'First Team to Score'][[1]]
        }

      output_list[[length(output_list) + 1]] <- first_team_to_score
    }
    if (prop %in% c('first player to score', 'fpts')) {

      # skip if no first basket props
      if (!'First Basket' %in% fixed_odds_markets$eventName) {
        next
      } else {
        first_basket <-
          fixed_odds_markets$outcomes[fixed_odds_markets$eventName == 'First Basket'][[1]]
      }

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

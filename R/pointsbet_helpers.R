parse_pb_prop <- function(game_event, fixed_odds_markets, event_names, prop_regex = NULL) {
  elements <- which(grepl(prop_regex, event_names))
  selected_markets <- fixed_odds_markets[elements]
  outcomes <- lapply(selected_markets, '[[', 'outcomes')
  outcomes_list <- list()
  for (o in outcomes) {
    extracted <- as.data.frame(do.call(rbind, o))
    outcomes_list[[length(outcomes_list) + 1]] <- extracted
  }
  output_df <- dplyr::bind_rows(outcomes_list)
  output_df$matchup <- game_event$name
  output_df$tipoff <- game_event$startsAt

  return(output_df)
}

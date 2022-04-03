parse_pb_prop <- function(game_event, fixed_odds_markets, event_names, prop_name = NULL, prop_regex = NULL) {
  if (!is.null(prop_name)) {
    elements <- which(event_names == prop_name)
  } else {
    elements <- which(grepl(prop_regex, event_names))
  }
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

parse_pb_main <- function(game_event, fixed_odds_markets, event_names) {
  main_output_list <- list()
  for (i in c('Point Spread', 'Moneyline', 'Total', 'Pick Your Own Line', 'Alternate Totals')) {
    mo <- parse_pb_prop(game_event = game_event, fixed_odds_markets = fixed_odds_markets, event_names = event_names, prop_name = i)
    main_output_list[[length(main_output_list) + 1]] <- mo
  }
  output_df <- dplyr::bind_rows(main_output_list)

  return(output_df)


}

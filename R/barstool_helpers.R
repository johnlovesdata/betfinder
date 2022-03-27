parse_bs_prop <- function(game_event, prop_name = NULL, prop_regex = NULL, matchup, tipoff) {
  label_vec <- unlist(lapply(game_event$betOffers, function(x) x[['criterion']][['label']]))
  if (!prop_name %in% label_vec) return()
  prop_content <- game_event$betOffers[which(label_vec == prop_name)]
  outcomes <- lapply(prop_content, '[[', 'outcomes')
  outcome_df <- as.data.frame(do.call(rbind, outcomes[[1]]))
  outcome_df$matchup <- matchup
  outcome_df$tipoff <- tipoff
  return(outcome_df)
}



parse_bs_main <- function(game_event, matchup, tipoff) {

  ml_outputs <- list()
  for (ml in c('Moneyline', 'Total Points', 'Point Spread')) {
    df <- parse_bs_prop(game_event, prop_name = ml, matchup = matchup, tipoff = tipoff)
    if (length(df) == 0) return()
    df$Type <- ml
    ml_outputs[[ml]] <- df
  }
  output_df <- dplyr::bind_rows(ml_outputs)
  return(output_df)

}


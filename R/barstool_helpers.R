parse_bs_prop <- function(game_event, prop_name = NULL, prop_regex = NULL, matchup, tipoff) {

  label_vec <- unlist(lapply(game_event$betOffers, function(x) x[['criterion']][['label']]))
  if (!prop_name %in% label_vec) return()
  prop_content <- game_event$betOffers[which(label_vec == prop_name)][[1]]
  outcomes <- prop_content$outcomes
    outcome_list <- list()
    for (o in outcomes) {
      outcome_list[[length(outcome_list) + 1]] <- unlist(o)
    }
    outcome_df <- dplyr::bind_rows(outcome_list)
    outcome_df$matchup <- matchup
    outcome_df$tipoff <- tipoff
    return(outcome_df)
}



parse_bs_main <- function(game_event, matchup, tipoff) {

  ml_outputs <- list()
  for (ml in c('Moneyline', 'Total Points', 'Point Spread')) {
    df <- parse_bs_prop(game_event, category_name = 'Most Popular', prop_name = ml, matchup = matchup, tipoff = tipoff)
    if (length(df) == 0) return()
    df$Type <- ml
    ml_outputs[[ml]] <- df
  }
  output_df <- dplyr::bind_rows(ml_outputs)
  return(output_df)

}


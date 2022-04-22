parse_mgm_prop <- function(game_event, prop_name = FALSE, prop_regex = NULL, prop_not_regex = NULL, matchup, tipoff) {

  label_vec <- game_event$games$name.value
  if (prop_name != FALSE & prop_name %in% label_vec) {
    prop_content <- game_event$games[which(label_vec == prop_name), ]
    outcomes_df <- as.data.frame(prop_content$results)

  }
  if (!is.null(prop_regex)) {
    prop_content <- game_event$games[grepl(prop_regex, label_vec), ]
    if (!is.null(prop_not_regex)) {
      prop_content <- prop_content[!grepl(prop_not_regex, prop_content$name.value), ]
    }

    outcomes_df <- dplyr::bind_rows(prop_content$results)
  }

  if (!('outcomes_df' %in% ls())) return()
  outcomes_df$matchup <- matchup
  outcomes_df$tipoff <- tipoff
  return(outcomes_df)
}


parse_mgm_main <- function(game_event, matchup, tipoff) {

  ml_outputs <- list()
  for (ml in c('Moneyline', 'Total Points', 'Point Spread')) {
    df <- parse_mgm_prop(game_event, prop_name = ml, matchup = matchup, tipoff = tipoff)
    if (length(df) == 0) return()
    df$Type <- ml
    ml_outputs[[ml]] <- df
  }
  output_df <- dplyr::bind_rows(ml_outputs)
  return(output_df)

}


parse_br_prop <- function(game_event, category_name, prop_name = NULL, prop_regex = NULL, matchup, tipoff) {

  ge_cat_names <- unlist(lapply(game_event$offeringGroups, '[[', 'categoryName'))
  if (!category_name %in% ge_cat_names) return()
  category_content <- game_event$offeringGroups[which(ge_cat_names == category_name)][[1]]
  # extract attachments
  if (!'criterionGroups' %in% names(category_content)) return()
  criterion_groups <- category_content$criterionGroups
  criterion_names <- unlist(lapply(criterion_groups, '[[', 'criterionName'))
  if (!is.null(prop_name)) {
    if (!prop_name %in% criterion_names) return()
    bet_offers <- criterion_groups[[which(criterion_names == prop_name)]]$betOffers
    outcome_list <- list()
    for (bo in bet_offers) {
      outcomes <- bo[['outcomes']]
      outcome_list[[length(outcome_list) + 1]] <- dplyr::bind_rows(outcomes)
    }
    outcome_df <- dplyr::bind_rows(outcome_list)
    outcome_df$matchup <- matchup
    outcome_df$tipoff <- tipoff
    return(outcome_df)
  }
}


parse_br_game_lines <- function(game_event, exclude_alts, matchup, tipoff) {
  gl_outputs <- list()
  for (i in c('Moneyline', 'Total Points', 'Point Spread')) {
    df <- parse_br_prop(game_event, category_name = 'Most Popular', prop_name = i, matchup = matchup, tipoff = tipoff)
    if (length(df) == 0) next
    df$Type <- i
    gl_outputs[[i]] <- df
  }
  output_df <- dplyr::bind_rows(gl_outputs)
  return(output_df)

}


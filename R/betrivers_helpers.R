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
    outcomes <- criterion_groups[[which(criterion_names == prop_name)]]$betOffers[[1]]$outcomes
    outcome_list <- list()
    for (o in outcomes) {
      row_df <- data.frame(
        label = o[['label']],
        participantName = o[['participantName']],
        oddsAmerican = as.numeric(o[['oddsAmerican']])
      )
      outcome_list[[length(outcome_list) + 1]] <- row_df
    }
    outcome_df <- do.call(rbind, outcome_list)
    outcome_df$matchup <- matchup
    outcome_df$tipoff <- tipoff
    return(outcome_df)
  }
}

#
# parse_br_main <- function(game_event, matchup, tipoff) {
#
#   ml_outputs <- list()
#   for (ml in c('Moneyline', 'Total Points', 'Spread Betting')) {
#     df <- parse_br_prop(game_event, category_name = 'main', prop_name = ml, matchup = matchup, tipoff = tipoff)
#     if (length(df) == 0) return()
#     df$Type <- ml
#     ml_outputs[[ml]] <- df
#   }
#   output_df <- do.call(rbind, ml_outputs)
#   return(output_df)
#
# }


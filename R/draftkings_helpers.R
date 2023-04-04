parse_dk_prop <- function(offer_categories, prop_group, prop_subgroup, prop_name = NULL, prop_regex = NULL, prop, matchup, tipoff) {
  offer_category_names <- unlist(lapply(offer_categories, '[[', 'name'))

  if (!prop_group %in% offer_category_names) return()
  prop_group_content <- offer_categories[[which(offer_category_names == prop_group)]]$componentizedOffers
  prop_group_names <- unlist(lapply(prop_group_content, '[[', 'subcategoryName'))
  if (!prop_subgroup %in% prop_group_names) return()
  prop_subgroup_content <- prop_group_content[[which(prop_group_names == prop_subgroup)]]$offers[[1]]
  prop_subgroup_names <- unlist(lapply(prop_subgroup_content, '[[', 'label'))
  # if there's just a single name for the prop, use prop_name

  if (!is.null(prop_name)) {

    if (!prop_name %in% prop_subgroup_names) return()
    prop_content <- prop_subgroup_content[[which(prop_subgroup_names == prop_name)]]
    # make a data.frame
    prop_df <- as.data.frame(do.call(rbind, prop_content$outcomes))
    prop_df$matchup <- matchup
    prop_df$tipoff <- tipoff
    return(prop_df)
  }

  if (!is.null(prop_regex)) {

    if (!any(grepl(prop_regex, prop_subgroup_names))) return()
    prop_content <- prop_subgroup_content[grepl(prop_regex, prop_subgroup_names)]
    prop_outcomes_list <- list()
    for (i in prop_content) {
      outcomes <- dplyr::bind_rows(i[['outcomes']])
      prop_outcomes_list[[length(prop_outcomes_list) + 1]] <- outcomes
    }
    prop_df <- dplyr::bind_rows(prop_outcomes_list)
    if ('participant' %in% names(prop_df)) {
      prop_df <- prop_df[!is.na(prop_df$participant), ]
    }
    if ('label' %in% names(prop_df)) {
      prop_df <- prop_df[!is.na(prop_df$label), ]
    }

    if(inherits(prop_df, 'list')) return()
    prop_df$matchup <- matchup
    prop_df$tipoff <- tipoff
    return(prop_df)
  }

}

parse_dk_game_lines <- function(offer_categories, offer_category = 'Game Lines', gl_subgroups = c('Game'), matchup, tipoff) {
  offer_category_names <- unlist(lapply(offer_categories, '[[', 'name'))
  if (!offer_category %in% offer_category_names) return()
  game_lines_content <- offer_categories[[which(offer_category_names == offer_category)]]$componentizedOffers

  gl_group_names <- unlist(lapply(game_lines_content, '[[', 'subcategoryName'))

  if (!any(gl_subgroups %in% gl_group_names)) return()
  gl_subgroups_content <- game_lines_content[which(gl_group_names %in% gl_subgroups)]
  gl_offers <- lapply(gl_subgroups_content, function(x) x[['offers']][[1]])
  output_list <- list()
  # go through each of the offer groups
  for (o in gl_offers) {
    outcomes <- lapply(o, '[[', 'outcomes')
    outcomes_df <- dplyr::bind_rows(outcomes)
    output_list[[length(output_list) + 1]] <- outcomes_df
  }

  out_df <- dplyr::bind_rows(output_list)
  if (nrow(out_df) < 1) return()

  out_df$bet_type <- ifelse(out_df$label %in% c("Over", "Under"), "Total",
                            ifelse(is.na(out_df$line), "Moneyline", "Spread"))

  out_df$matchup <- matchup
  out_df$tipoff <- tipoff
  return(out_df)
}


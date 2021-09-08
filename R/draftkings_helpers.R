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
      prop_outcomes_list[[length(prop_outcomes_list) + 1]] <- as.data.frame(do.call(rbind, i[['outcomes']]))
    }
    prop_df <- as.data.frame(do.call(rbind, prop_outcomes_list))

    if(inherits(prop_df, 'list')) return()
    prop_df$matchup <- matchup
    prop_df$tipoff <- tipoff
    return(prop_df)
  }

}


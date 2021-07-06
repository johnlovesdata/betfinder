#' write out the data for the props dashboard
#' @importFrom magrittr %>%
#' @param data_list list of non-props dfs
#' @param props_list list of props dfs
#' @param output_path path to save output
#' @return nothing
#' @export
output_data_for_dashboard <- function(data_list, props_list,
                                      output_path = system.file('props.rds', package = 'betfinder')) {

  # combine all the props
  props_long <- list()
  for (i in props_list) {
    if(inherits(i, 'list')) next
    if(nrow(i) == 0) next
    df <- i %>% dplyr::filter(!is.infinite(tidyamericanodds))
    props_long[[length(props_long) + 1]] <- df
  }
  props_long <- dplyr::bind_rows(props_long)
  # handle missing columns - this should never happen if there are actually data i think?
  ## TODO: DECIDE WHETHER THIS IS NECESSARY OR EVEN A GOOD IDEA
  if (!'tidygamedatetime' %in% names(props_long)) props_long$tidygamedatetime <- NA_character_
  if (!'tidyplayer' %in% names(props_long)) props_long$tidyplayer <- NA_character_
  if (!'tidyteam' %in% names(props_long)) props_long$tidyteam <- NA_character_
  if (!'tidyhometeam' %in% names(props_long)) props_long$tidyhometeam <- NA_character_
  if (!'tidyawayteam' %in% names(props_long)) props_long$tidyawayteam <- NA_character_
  if (!'tidyou' %in% names(props_long)) props_long$tidyou <- NA_character_
  if (!'tidyline' %in% names(props_long)) props_long$tidyline <- NA_real_

  props_wide <- tidyr::pivot_wider(
    data = props_long,
    id_cols = c(sport, prop, tidygamedatetime, tidyplayer, tidyteam, tidyhometeam, tidyawayteam, tidyou, tidyline),
    names_from = site,
    # values_fn = length,
    values_from = tidyamericanodds)
  # handle missing columns - this should happen occasionally, but should probably be handled via a function that can read configs
  ## TODO: update configs to make this work functionally
  if (!'draftkings' %in% names(props_wide)) props_wide$draftkings <- NA_real_
  if (!'fanduel' %in% names(props_wide)) props_wide$fanduel <- NA_real_
  if (!'pointsbet' %in% names(props_wide)) props_wide$pointsbet <- NA_real_

  # merge external stuff ----
  merged <- props_wide %>%
    # player data; EXCLUDE TEAM FROM THE MERGE AND COALESCE IT IN
    dplyr::left_join(data_list$rosters, by = c('sport', 'tidyplayer')) %>%
    dplyr::mutate(tidyteam = dplyr::coalesce(tidyteam.x, tidyteam.y)) %>%
    dplyr::select(-dplyr::ends_with('.x'), -dplyr::ends_with('.y')) %>%
    # projections
    dplyr::left_join(data_list$projections,
                     by = c("sport", "prop", "tidyplayer", "tidyteam"))
  # tidy up ----
  # book-level conversions
  props_df <- merged %>%
    dplyr::mutate(
      tidyopp = dplyr::if_else(tidyteam == tidyhometeam, tidyawayteam, tidyhometeam),
      home_away = dplyr::if_else(tidyteam == tidyhometeam, 'home', 'away')) %>%
    # fill in blank opponents for team props
    dplyr::group_by(tidyteam) %>%
    tidyr::fill(tidyopp, home_away, .direction = 'downup') %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      # recode NAs in tidyou (KEEP IT THIS WAY, YOU CHANGED IT ONCE ALREADY AND IT WAS A BAD DECISION)
      tidyou = dplyr::if_else(is.na(tidyou), 'N/A', tidyou)) %>%
    # convert odds, calculate deltas with projections
    dplyr::mutate(
      dplyr::across(
        c(draftkings, fanduel, pointsbet),
        round)) %>%
    dplyr::mutate(dk_prob = american_to_prob(draftkings),
                  fd_prob = american_to_prob(fanduel),
                  pb_prob = american_to_prob(pointsbet)) %>%
    dplyr::mutate(dk_delta = projected_prob - dk_prob,
                  fd_delta = projected_prob - fd_prob,
                  pb_delta = projected_prob - pb_prob) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(count_books = sum(!is.na(c(draftkings, fanduel, pointsbet))),
                  mean_prob = mean(c(dk_prob, fd_prob, pb_prob), na.rm = TRUE),
                  mean_odds = prob_to_american(mean_prob),
                  worst_prob = max(c(dk_prob, fd_prob, pb_prob), na.rm = TRUE),
                  best_prob = min(c(dk_prob, fd_prob, pb_prob), na.rm = TRUE),
                  best_odds = prob_to_american(best_prob),
                  probs_list = list(c(dk_prob, fd_prob, pb_prob)),
                  best_delta = max(c(dk_delta, fd_delta, pb_delta), na.rm = TRUE),
                  best_delta = dplyr::if_else(is.infinite(best_delta), NA_real_, best_delta)) %>%
    dplyr::ungroup()

  # additional row-wise calculations
  best_books <- list()
  next_best_probs <- list()

  for (i in 1:nrow(props_df)) {
    ## subset row
    sel_row <- props_df[i, ]
    ## get all the probabilities
    probs_vec <- na.omit(unlist(sel_row$probs_list))
    ### get the next best probabilities when there are more than 1 unique value
    next_best_probs[[length(next_best_probs) + 1]] <-
      ifelse(length(unique(probs_vec)) == 1, NA_real_, min(probs_vec[probs_vec > min(probs_vec)]))

    # get the best odds in a nicely formatted character string
    best <- sel_row$best_odds
    sel_row$mean_odds <- NULL
    sel_row$best_odds <- NULL
    bb <- sort(as.character(na.omit(names(sel_row)[unlist(sel_row) == best])))
    best_books[[length(best_books) + 1]] <- paste0(bb, collapse = ', ')

  }
  props_df <- props_df %>%
    dplyr::select(-probs_list) %>%
    dplyr::mutate(best_books = unlist(best_books),
                  next_best_prob = unlist(next_best_probs),
                  next_best_ratio = dplyr::case_when(count_books == 1 ~ NA_real_,
                                                     count_books == 2 ~ best_prob / worst_prob,
                                                     count_books > 2 ~ best_prob / next_best_prob),
                  projected_odds = prob_to_american(projected_prob)) %>%
    dplyr::mutate(best_books = gsub('draftkings, fanduel, pointsbet', 'dk, fd, pb', best_books)) %>%
    dplyr::mutate(
      tipoff_string = paste0(
        weekdays(tidygamedatetime, abbreviate = TRUE), ' ',
        gsub(' 0', ' ', as.character(format(tidygamedatetime, format = '%m-%d %I:%M %p %Z')))
      ))
  # stash the datetime when these data were last updated
  attr(props_df, 'timestamp') <- Sys.time()
  # save dash output
  saveRDS(props_df, output_path)

}

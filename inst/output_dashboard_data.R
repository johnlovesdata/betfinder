# combine all the props ----
props_long <- bind_rows(props_list) %>%
  filter(!is.infinite(tidyamericanodds))
# handle missing columns
if (!'tidygamedatetime' %in% names(props_long)) props_long$tidygamedatetime <- NA_character_
if (!'tidyplayer' %in% names(props_long)) props_long$tidyplayer <- NA_character_
if (!'tidyteam' %in% names(props_long)) props_long$tidyteam <- NA_character_
if (!'tidyhometeam' %in% names(props_long)) props_long$tidyhometeam <- NA_character_
if (!'tidyawayteam' %in% names(props_long)) props_long$tidyawayteam <- NA_character_
if (!'tidyou' %in% names(props_long)) props_long$tidyou <- NA_character_
if (!'tidyline' %in% names(props_long)) props_long$tidyline <- NA_real_
# make the props wider ----
props_wide <- pivot_wider(
  data = props_long,
  id_cols = c(sport, prop, tidygamedatetime, tidyplayer, tidyteam, tidyhometeam, tidyawayteam, tidyou, tidyline),
  names_from = site,
  values_from = tidyamericanodds)
# handle missing columns
if (!'draftkings' %in% names(props_wide)) props_wide$draftkings <- NA_real_
if (!'fanduel' %in% names(props_wide)) props_wide$fanduel <- NA_real_
if (!'pointsbet' %in% names(props_wide)) props_wide$pointsbet <- NA_real_

# merge external stuff ----
merged <- props_wide %>%
  # player data
  left_join(player_data, by = "tidyplayer") %>%
  mutate(tidyteam = coalesce(tidyteam.x, tidyteam.y)) %>%
  select(-tidyteam.x, -tidyteam.y) %>%
  left_join(projections) %>%
  mutate(injury_status = coalesce(injury_status, jumper_injury_status))

# tidy up ----
# book-level conversions
props_df <- merged %>%
  mutate(tidyopp = if_else(tidyteam == tidyhometeam, tidyawayteam, tidyhometeam),
         home_away = if_else(tidyteam == tidyhometeam, 'home', 'away')) %>%
  # fill in blank opponents for team props
  group_by(tidyteam) %>%
  fill(tidyopp, home_away, .direction = 'downup') %>%
  ungroup() %>%
  mutate(
    # recode NAs in tidyou (KEEP IT THIS WAY, YOU CHANGED IT ONCE ALREADY AND IT WAS A BAD DECISION)
    tidyou = if_else(is.na(tidyou), 'N/A', tidyou)) %>%
  # convert odds, calculate deltas with projections
  mutate(across(
      c(draftkings, fanduel, pointsbet),
      round)) %>%
  mutate(dk_prob = american_to_prob(draftkings),
         fd_prob = american_to_prob(fanduel),
         pb_prob = american_to_prob(pointsbet)) %>%
  mutate(dk_delta = projected_prob - dk_prob,
         fd_delta = projected_prob - fd_prob,
         pb_delta = projected_prob - pb_prob) %>%
  rowwise() %>%
  mutate(count_books = sum(!is.na(c(draftkings, fanduel, pointsbet))),
         mean_prob = mean(c(dk_prob, fd_prob, pb_prob), na.rm = TRUE),
         mean_odds = prob_to_american(mean_prob),
         worst_prob = max(c(dk_prob, fd_prob, pb_prob), na.rm = TRUE),
         best_prob = min(c(dk_prob, fd_prob, pb_prob), na.rm = TRUE),
         best_odds = prob_to_american(best_prob),
         probs_list = list(c(dk_prob, fd_prob, pb_prob)),
         odds_list = list(c(draftkings, fanduel, pointsbet)),
         best_delta = max(c(dk_delta, fd_delta, pb_delta), na.rm = TRUE),
         best_delta = if_else(is.infinite(best_delta), NA_real_, best_delta)) %>%
  ungroup()

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
  sel_row$odds_list <- NULL
  bb <- sort(as.character(na.omit(names(sel_row)[unlist(sel_row) == best])))
  best_books[[length(best_books) + 1]] <- paste0(bb, collapse = ', ')

}
props_df <- props_df %>%
  select(-probs_list,
         -odds_list) %>%
  mutate(best_books = unlist(best_books),
         next_best_prob = unlist(next_best_probs),
         next_best_ratio = case_when(count_books == 1 ~ NA_real_,
                                     count_books == 2 ~ best_prob / worst_prob,
                                     count_books > 2 ~ best_prob / next_best_prob),
         projected_odds = prob_to_american(projected_prob))
# NEW ----
## add a date string for tipoffs cuz it's ugly af otherwise rendering in the dash
props_df <- props_df %>%
  mutate(
    tipoff_string = paste0(
      weekdays(as.Date(tidygamedatetime), abbreviate = TRUE), ' ',
      gsub(' 0', ' ', as.character(format(tidygamedatetime, format = '%m-%d %I:%M %p %Z')))
  ))
# stash the datetime when these data were last updated
attr(props_df, 'timestamp') <- Sys.time()
# save dash output
saveRDS(props_df, 'inst/props.rds')


tidyup_fanduel_data <- function(fanduel_data, sport, prop,
                                key = get_key_path(sport = sport, prop = prop)) {

  # make the output using the input
  output_df <- fanduel_data

  # for each prop, append whatever tidy fields we can, which should make thte data useful across datasets
  if (prop %in% c('first team to score', 'ftts')) {
    # generate tidy names and odds
    output_df$tidyplayer <- 'team'
    output_df$tidyteam <- normalize_names(output_df$team, key = key)
    output_df$tidyamericanodds <- as.numeric(output_df$american_odds)
    output_df$prop <- 'first team to score'
  }

  if (prop %in% c('first player to score', 'fpts')) {
    # generate tidy names and odds
    hacky_tidyplayer <- hacky_tidyup_player_names(output_df$player)
    output_df$tidyplayer <- normalize_names(hacky_tidyplayer, key = key)
    output_df$tidyamericanodds <- as.numeric(output_df$american_odds)
    output_df$prop <- 'first player to score'
  }

  if (grepl('points|rebounds|assists|three| 3pts| pts| rebs| asts', tolower(prop))) {

    # handle special cases by prop type (alt, ou, tiers)
    if (grepl('alt$', tolower(prop))) {
      ## alt lines can be over or under, but need to extract direction and line from names
      output_df$tidyou <- ifelse(grepl('Over', as.character(output_df$prop)), 'over', 'under')
      ## get the name AND line out of the name; split everything first to make this easier
      split_string <- gsub(' Over | Under ', 'XX', as.character(output_df$prop))
      splitted <- strsplit(split_string, 'XX')
      splitted_name <- sapply(splitted, '[[', 1)
      splitted_name <- hacky_tidyup_player_names(splitted_name)
      splitted_line <- sapply(splitted, '[[', 2)
      output_df$tidyplayer <- normalize_names(splitted_name, key = key)
      output_df$tidyline <- as.numeric(splitted_line)
      # set odds
      output_df$tidyamericanodds <- as.numeric(output_df$american_odds)
    }
    if (grepl('ou$', tolower(prop))) {
      ## extract player name and ou from the prop field
      prop_string <- as.character(output_df$prop)
      player_part <- gsub(' Over$| Under$', '', prop_string)
      hacky_player <- hacky_tidyup_player_names(player_part)
      output_df$tidyplayer <- normalize_names(hacky_player, key = key)
      output_df$tidyou <- ifelse(grepl('Over', output_df$prop), 'over', 'under')
      ## set odds
      output_df$tidyamericanodds <- output_df$american_odds
    }

    ## tiers are always overs, but the lines are in the prop_details, not the handicap
    if (grepl('tiers', tolower(prop))) {
      output_df$tidyou <- 'over'
      hacky_tidyplayer <- hacky_tidyup_player_names(gsub(' Over| Under', '', as.character(output_df$player)))
      output_df$tidyplayer <- normalize_names(hacky_tidyplayer, key = key)
      # as kyle mentioned, tiers are >= values, so if we're calling it an over need to subtract half a point
      output_df$tidyline <- as.numeric(gsub('[A-Za-z| |+]', '', output_df$prop)) - .5
      output_df$tidyamericanodds <- as.numeric(output_df$american_odds)
    }

    # fix the prop name to be whatever the input arg is
    output_df$prop <- prop

    # handle any tidy values that weren't already handled
    ## if tidyplayer isn't set, set it
    if (!'tidyplayer' %in% names(output_df)) {
      hacky_tidyplayer <- hacky_tidyup_player_names(as.character(output_df$player))
      output_df$tidyplayer <- normalize_names(hacky_tidyplayer, key = key)
    }
    ## if tidyline isn't set, set it
    if (!'tidyline' %in% names(output_df)) {
      output_df$tidyline <- output_df$handicap
    }
    ## if the ou column doesn't exist, make it exist but NA_character
    if (!'tidyou' %in% names(output_df)) {
      output_df$tidyou <- NA_character_
    }
  }

  # tidyup the matchup! use the team abbreviations from the lookup
  matchup_list <- strsplit(output_df$matchup, ' @ ')
  output_df$tidyawayteam <- normalize_names(unlist(lapply(matchup_list, '[[', 1)), key = get_key_path(sport, 'team'))
  output_df$tidyhometeam <- normalize_names(unlist(lapply(matchup_list, '[[', 2)), key = get_key_path(sport, 'team'))

  # tidyup the date! make sure this is EST
  output_df$tidygamedatetime <- lubridate::as_datetime(output_df$tipoff) - lubridate::hours(4)
  output_df$tidygamedatetime <- lubridate::round_date(output_df$tidygamedatetime, "30 minutes")
  lubridate::tz(output_df$tidygamedatetime) <- 'EST'

  # filter out the cols we don't need, i.e. not tidy ones
  names_to_keep <- names(output_df)[grepl('tidy|prop', names(output_df))]
  output_df <- output_df[, names(output_df) %in% names_to_keep]

  # stamp it up
  output_df$site <- 'fanduel'
  output_df$sport <- sport
  if (!'prop' %in% names(output_df)) {
    output_df$prop <- prop
  }

  # deliver
  return(output_df)
}

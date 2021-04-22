tidyup_fanduel_data <- function(fanduel_data, sport, prop,
                                key = get_key_path(sport = sport, prop = prop)) {

  # make the output using the input
  output_df <- fanduel_data

  # for each prop, append whatever tidy fields we can, which should make thte data useful across datasets
  if (prop %in% c('first team to score', 'ftts')) {

    # generate tidy names and odds
    output_df$tidyteam <- normalize_names(output_df$name, key = key)
    ## need to parse the description to get the opponent
    splitted <- strsplit(output_df$description, ' At ')
    home <- sapply(splitted, '[[', 1)
    away <- sapply(splitted, '[[', 2)
    output_df$opponent <- ifelse(output_df$name == home, away, home)
    output_df$tidyopponent <- normalize_names(output_df$opponent, key = key)
    ## odds are actually in fractional across 2 fields
    fractional_odds <- output_df$currentpriceup / output_df$currentpricedown
    output_df$tidyamericanodds <- ifelse(fractional_odds < 1, -100 / fractional_odds,
                                         fractional_odds * 100)
    # since prop arg is flexible, set it here for output
    if (prop == 'ftts') {
      output_df$prop <- 'first team to score'
    }
  }

  if (prop %in% c('first player to score', 'fpts')) {

    hacky_tidyplayer <- hacky_tidyup_player_names(output_df$name)
    output_df$tidyplayer <- normalize_names(hacky_tidyplayer, key = key)
    fractional_odds <- output_df$currentpriceup / output_df$currentpricedown
    output_df$tidyamericanodds <- ifelse(fractional_odds < 1, -100 / fractional_odds,
                                         fractional_odds * 100)
    # since prop arg is flexible, set it here for output
    if (prop == 'fpts') {
      output_df$prop <- 'first player to score'
    }
  }

  if (grepl('points|rebounds|assists| pts| rebs| asts', tolower(prop))) {
    # handle special cases by prop type
    ## alt lines can be over or under, but need to extract direction and line from names
    if (grepl('alt$', tolower(prop))) {
      ## set the over/under column values
      output_df$tidyou <- ifelse(grepl('Over', output_df$name), 'over', 'under')
      ## get the name AND line out of the name; split everything first to make this easier
      split_string <- gsub(' Over | Under ', 'XX', output_df$name)
      splitted <- strsplit(split_string, 'XX')
      splitted_name <- sapply(splitted, '[[', 1)
      splitted_name <- hacky_tidyup_player_names(splitted_name)
      splitted_line <- sapply(splitted, '[[', 2)

      output_df$tidyplayer <- normalize_names(splitted_name, key = key)
      output_df$tidyline <- as.numeric(splitted_line)
    }
    if (grepl('ou$', tolower(prop))) {
      ## set the over/under column values
      output_df$tidyou <- ifelse(grepl('Over', output_df$name), 'over', 'under')
      ## the ou player names still have over and under in them, so nuke those
      output_df$name <- gsub(' Over| Under', '', output_df$name)
    }

    ## tiers are always overs, but the lines are in the prop_details, not the handicap
    if (grepl('tiers', tolower(prop))) {
      output_df$tidyou <- 'over'
      output_df$tidyline <- as.numeric(gsub('[A-Za-z| |+]', '', output_df$prop_details))
      output_df$prop_details <- NULL
    }

    # convert the odds
    fractional_odds <- output_df$currentpriceup / output_df$currentpricedown
    output_df$tidyamericanodds <- ifelse(fractional_odds < 1, -100 / fractional_odds,
                                         fractional_odds * 100)

    # handle any tidy values that weren't already handled
    ## if tidyplayer isn't set, set it
    if (!'tidyplayer' %in% names(output_df)) {
      hacky_tidyplayer <- hacky_tidyup_player_names(output_df$name)
      output_df$tidyplayer <- normalize_names(hacky_tidyplayer, key = key)
    }
    ## if tidyline isn't set, set it
    if (!'tidyline' %in% names(output_df)) {
      output_df$tidyline <- output_df$currenthandicap
    }
    ## if the ou column doesn't exist, make it exist but NA_character
    if (!'tidyou' %in% names(output_df)) {
      output_df$tidyou <- NA_character_
    }
  }

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

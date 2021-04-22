tidyup_pointsbet_data <- function(pointsbet_data, sport, prop,
                                  key = get_key_path(sport = sport, prop = prop)) {

  # make the output from the input
  output_df <- pointsbet_data

  # for each prop, append tidy team, tidy opponent, tidy odds (numeric american odds)
  if (prop %in% c('first team to score', 'ftts')) {

    # generate tidy names and odds
    output_df$tidyteam <- normalize_names(output_df$name, key = key)
    ## need to infer opponent by using the groupByCode
    splitted <- split.data.frame(output_df, output_df$groupByCode)
    dfs_with_opps <- list()
    for (group in splitted) {
      group$tidyopponent <- rev(group$tidyteam)
      dfs_with_opps[[length(dfs_with_opps) + 1]] <- group
    }
    output_df <- do.call(rbind, dfs_with_opps)
    ## odds are decimal
    output_df$tidyamericanodds <- ifelse(output_df$price - 1 < 1,
                                         -100 / (output_df$price - 1),
                                         (output_df$price - 1) * 100)
    # since prop arg is flexible, set it here for output
    output_df$prop <- 'first team to score'
  }

  if (prop %in% c('first player to score', 'fpts')) {
    hacky_tidyplayer <- hacky_tidyup_player_names(output_df$name)
    output_df$tidyplayer <- normalize_names(hacky_tidyplayer, key = key)
    output_df$tidyamericanodds <- ifelse(output_df$price - 1 < 1,
                                         -100 / (output_df$price - 1),
                                         (output_df$price - 1) * 100)
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
      ## get the name AND line out of the name; split everything first to make this easier
      split_string <- gsub(' Over | Under ', 'XX', output_df$name)
      splitted <- strsplit(split_string, 'XX')

      splitted_name <- sapply(splitted, '[[', 1)
      splitted_name <- hacky_tidyup_player_names(splitted_name)
      output_df$tidyplayer <- normalize_names(splitted_name, key = key)

      splitted_line <- sapply(splitted, '[[', 2)
      splitted_line <- gsub('[A-Za-z| |+]', '', splitted_line)
      output_df$tidyline <- as.numeric(splitted_line)
    }
    ## tiers are always overs, but the lines are in the prop_details, not the handicap
    if (grepl('tiers', tolower(prop))) {
      output_df$tidyou <- 'over'
      ## get the name AND line out of the name; split everything first to make this easier
      split_string <- gsub(' To Get ', 'XX', output_df$name)
      splitted <- strsplit(split_string, 'XX')
      splitted_name <- sapply(splitted, '[[', 1)
      splitted_name <- hacky_tidyup_player_names(splitted_name)
      splitted_line <- sapply(splitted, '[[', 2)

      output_df$tidyplayer <- normalize_names(splitted_name, key = key)
      output_df$tidyline <- as.numeric(gsub('[^0-9]', '', splitted_line))
    }

    # set the odds
    output_df$tidyamericanodds <- ifelse(output_df$price - 1 < 1,
                                         -100 / (output_df$price - 1),
                                         (output_df$price - 1) * 100)

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

  # keep the tidy columns
  names_to_keep <- names(output_df)[grepl('tidy|prop', names(output_df))]
  output_df <- output_df[, names(output_df) %in% names_to_keep]

  # stamp it up
  output_df$site <- 'pointsbet'
  output_df$sport <- sport
  if (!'prop' %in% names(output_df)) {
    output_df$prop <- prop
  }

  return(output_df)
}

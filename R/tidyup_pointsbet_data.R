tidyup_pointsbet_data <- function(pointsbet_data, sport, prop = FALSE, game_lines = FALSE,
                                  key = get_key_path(sport = sport, prop = prop, game_lines = game_lines)) {
  if (nrow(pointsbet_data) < 1) stop('no pointsbet ', prop, ' available')
  # make the output from the input
  output_df <- pointsbet_data
  # game_lines
  if (game_lines == TRUE){
    # fix the totals first
    totals <- output_df[output_df$groupByHeader == 'Total', ]
    new_totals_list <- list()
    for (m in unique(totals$matchup)) {
      mu <- totals[totals$matchup == m, ]
      teams <- unlist(strsplit(m, ' @ '))
      mu$name[[1]] <- teams[[1]]
      mu$name[[2]] <- teams[[2]]
      new_totals_list[[length(new_totals_list) + 1]] <- mu
    }
    new_totals <- dplyr::bind_rows(new_totals_list)
    output_df <- dplyr::bind_rows(new_totals, output_df[output_df$groupByHeader != 'Total', ])
    output_df$groupByHeader <- gsub("Point ", "", output_df$groupByHeader)
    # then fix the rest of names
    all_names <- as.character(output_df$name)
    all_names <- gsub('\\+', '-', all_names)
    split_names <- strsplit(all_names, ' -')
    new_names <- unlist(lapply(split_names, '[[', 1))
    output_df$name <- new_names

    output_df$tidyteam <- normalize_names(as.character(output_df$name), key = key)
    output_df$tidyplayer <- 'team'
    output_df$tidytype <- as.character(output_df$groupByHeader)
    output_df$tidyline <- as.numeric(output_df$points)
    output_df$tidyou <- ifelse(output_df$outcomeType %in% c('Over', 'Under'), tolower(output_df$outcomeType), NA_character_)
    output_df$tidyamericanodds <- ifelse(as.numeric(output_df$price) - 1 < 1,
                                         -100 / (as.numeric(output_df$price) - 1),
                                         (as.numeric(output_df$price) - 1) * 100)
  }

  # for each prop, append tidy team, tidy opponent, tidy odds (numeric american odds)
  if (prop %in% c('first team to score', 'ftts')) {
    # generate tidy names and odds
    output_df$tidyteam <- normalize_names(as.character(output_df$name), key = key)
    output_df$tidyplayer <- 'team'
    output_df$tidyamericanodds <- ifelse(as.numeric(output_df$price) - 1 < 1,
                                         -100 / (as.numeric(output_df$price) - 1),
                                         (as.numeric(output_df$price) - 1) * 100)
    # since prop arg is flexible, set it here for output
    output_df$prop <- 'first team to score'
  }
  if (prop %in% c('first player to score', 'fpts')) {
    hacky_tidyplayer <- hacky_tidyup_player_names(as.character(output_df$name))
    output_df$tidyplayer <- normalize_names(hacky_tidyplayer, key = key)
    output_df$tidyamericanodds <- ifelse(as.numeric(output_df$price) - 1 < 1,
                                         -100 / (as.numeric(output_df$price) - 1),
                                         (as.numeric(output_df$price) - 1) * 100)
    # since prop arg is flexible, set it here for output
    output_df$prop <- 'first player to score'
  }
  if (prop %in% c('player first td', 'player any td')) {
    hacky_tidyplayer <- hacky_tidyup_player_names(as.character(output_df$name))
    output_df$tidyplayer <- normalize_names(hacky_tidyplayer, key = key)
    output_df$tidyamericanodds <- ifelse(as.numeric(output_df$price) - 1 < 1,
                                         -100 / (as.numeric(output_df$price) - 1),
                                         (as.numeric(output_df$price) - 1) * 100)
    # since prop arg is flexible, set it here for output
    output_df$prop <- prop
  }
  if (grepl('alt$| ou$|tiers$|points|rebounds|assists|three| 3pts| pts| rebs| asts|hit|double|pass|rush|rec', tolower(prop))) {
    # handle special cases by prop type
    ## alt lines can be over or under, but need to extract direction and line from names
    if (grepl('alt$', tolower(prop))) {
      ## set the over/under column value, which is always an over
      output_df$tidyou <- 'over'
      ## get the name AND line out of the name; split everything first to make this easier
      split_string <- gsub(' To Get | To Make | To Record ', 'XX', as.character(output_df$name))
      splitted <- strsplit(split_string, 'XX')
      splitted_name <- sapply(splitted, '[[', 1)
      splitted_name <- hacky_tidyup_player_names(splitted_name)
      splitted_line <- gsub('[A-Za-z |\\+]', '', sapply(splitted, '[[', 2))
      output_df$tidyplayer <- normalize_names(splitted_name, key = key)
      # the lines here are for "N+ made 3s" so adjust for that here by subtracting half a point
      output_df$tidyline <- as.numeric(splitted_line) - .5
    }
    if (grepl('ou$', tolower(prop))) {
      ## set the over/under column values
      output_df$tidyou <- ifelse(grepl('Over', as.character(output_df$name)), 'over', 'under')
      ## get the name AND line out of the name; split everything first to make this easier
      split_string <- gsub(' Over | Under | over | under ', 'XX', as.character(output_df$name))
      splitted <- strsplit(split_string, 'XX')

      splitted_name <- sapply(splitted, '[[', 1)
      splitted_name <- hacky_tidyup_player_names(splitted_name)
      output_df$tidyplayer <- normalize_names(splitted_name, key = key)

      splitted_line <- sapply(splitted, '[[', 2)
      splitted_line <- gsub('[A-Za-z| |+]', '', splitted_line)
      output_df$tidyline <- as.numeric(splitted_line)
    }
    ## tiers are always overs, but the lines are in the prop_details, not the handicap
    if (grepl('tiers|double', tolower(prop))) {
      output_df$tidyou <- 'over'
      ## get the name AND line out of the name; split everything first to make this easier
      split_string <- gsub(' To Make | To Get | To Get [Aa]', 'XX', output_df$name)
      splitted <- strsplit(split_string, 'XX')
      splitted_name <- sapply(splitted, '[[', 1)
      splitted_name <- hacky_tidyup_player_names(splitted_name)
      splitted_line <- sapply(splitted, '[[', 2)
      output_df$tidyplayer <- normalize_names(splitted_name, key = key)
      # as kyle pointed out, tiers are "score at least lines" so need to cut half a point from them
      output_df$tidyline <- as.numeric(gsub('[^0-9]', '', splitted_line)) - .5
    }

    # set the odds
    output_df$tidyamericanodds <- ifelse(as.numeric(output_df$price) - 1 < 1,
                                         -100 / (as.numeric(output_df$price) - 1),
                                         (as.numeric(output_df$price) - 1) * 100)

    # handle any tidy values that weren't already handled
    ## if tidyplayer isn't set, set it
    if (!'tidyplayer' %in% names(output_df)) {
      hacky_tidyplayer <- hacky_tidyup_player_names(unlist(output_df$name))
      output_df$tidyplayer <- normalize_names(hacky_tidyplayer, key = key)
    }
    ## if tidyline isn't set, set it
    if (!'tidyline' %in% names(output_df) && 'currenthandicap' %in% names(output_df)) {
      output_df$tidyline <- unlist(output_df$currenthandicap)
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

tidyup_pointsbet_data <- function(pointsbet_data, sport, prop = FALSE, game_lines = FALSE,
                                  key = get_key_path(sport = sport, prop = prop, game_lines = game_lines)) {


  if (nrow(pointsbet_data) < 1) stop('no pointsbet ', prop, ' available')
  # make the output from the input
  output_df <- pointsbet_data

  # game_lines
  if (game_lines == TRUE){
    # # fix the totals first
    # totals <- output_df[grepl('Total', output_df$groupByHeader), ]
    # new_totals_list <- list()
    # for (m in unique(totals$matchup)) {
    #   mu <- totals[totals$matchup == m, ]
    #   teams <- unlist(strsplit(m, ' @ '))
    #   mu$name[[1]] <- teams[[1]]
    #   mu$name[[2]] <- teams[[2]]
    #   new_totals_list[[length(new_totals_list) + 1]] <- mu
    # }
    # new_totals <- dplyr::bind_rows(new_totals_list)
    # output_df <- dplyr::bind_rows(new_totals, output_df[!grepl('Total', output_df$groupByHeader), ])

    ## split the names string to nuke the handicaps from string
    name_tosplit <- gsub(' \\+| \\-', 'xxx', output_df$name)
    name_split <- strsplit(name_tosplit, 'xxx')
    name_split <- as.character(lapply(name_split, function(x) x[[1]]))
    output_df$name <- name_split

    # create standard fields
    output_df$newname <- ifelse(grepl("^Over |^Under ", output_df$name), NA_character_, output_df$name)
    output_df$tidyteam <- normalize_names(as.character(output_df$newname), key = key, warn = FALSE)
    output_df$tidyplayer <- 'team'
    output_df$tidytype <- ifelse(grepl("Total", output_df$groupByHeader), "Total",
                                 ifelse(grepl("Moneyline", output_df$groupByHeader), "Moneyline", "Spread"))
    output_df$tidyline <- as.numeric(output_df$points)
    if ('outcomeType' %in% names(output_df)) {
      output_df$tidyou <- ifelse(grepl("^Over", output_df$name), "over",
                                 ifelse(grepl("^Under", output_df$name), "under", NA_character_))
    }
    if (!'outcomeType' %in% names(output_df) & 'marketTypeCode' %in% names(output_df)) {
      output_df$tidyou <- ifelse(grepl('OVER', output_df$marketTypeCode), 'over',
                                 ifelse(grepl('UNDER', output_df$marketTypeCode), 'under', NA_character_))
    }


    output_df$tidyamericanodds <- ifelse(as.numeric(output_df$price) - 1 < 1,
                                         -100 / (as.numeric(output_df$price) - 1),
                                         (as.numeric(output_df$price) - 1) * 100)
  }

  # for each prop, append tidy team, tidy opponent, tidy odds (numeric american odds)
  if (prop %in% c('game made first fg', 'game go to overtime', 'game go to ot')) {
    if (output_df$prop == 'game go to overtime') output_df$prop <- 'game go to ot'
    output_df$tidyteam <- 'game'
    output_df$tidyplayer <- 'game'
    output_df$tidyou <- output_df$name
    output_df$tidyamericanodds <- ifelse(as.numeric(output_df$price) - 1 < 1,
                                         -100 / (as.numeric(output_df$price) - 1),
                                         (as.numeric(output_df$price) - 1) * 100)
    # since prop arg is flexible, set it here for output
    output_df$prop <- output_df$prop
  }

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

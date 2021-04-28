tidyup_draftkings_data <- function(draftkings_data, sport, prop,
                                   key = get_key_path(sport = sport, prop = prop)) {

  # make the output using the input
  output_df <- draftkings_data

  # for each prop, append whatever tidy fields we can, which should make thte data useful across datasets
  if (grepl('team|ftts', prop)) {
    # generate tidy names and odds
    output_df$tidyteam <- normalize_names(output_df$label, key = key)
    output_df$tidyplayer <- 'team'
    output_df$tidyamericanodds <- as.numeric(output_df$oddsAmerican)

    # for flexible prop names, specify the value explicitly here
    if (prop == 'ftts') {
      output_df$prop <- 'first team to score'
    }
  }

  if (prop %in% c('first player to score', 'fpts')) {
    # set tidyplayer and tidyamericanodds
    hacky_tidyplayer <- hacky_tidyup_player_names(as.character(output_df$participant))
    output_df$tidyplayer <- normalize_names(hacky_tidyplayer, key = key)
    output_df$tidyamericanodds <- as.numeric(output_df$oddsAmerican)
    # for flexible props, specify the value explicitly here
    if (prop == 'fpts') output_df$prop <- 'first player to score'
  }

  if (grepl('points|rebounds|assists|three-pointers| pts| 3pts| rebs| asts', tolower(prop))) {

    # get names
    hacky_tidyplayer <- hacky_tidyup_player_names(output_df$participant)
    output_df$tidyplayer <- normalize_names(hacky_tidyplayer, key = key)
    # get tidy ou from label
    output_df$tidyou <- ifelse(grepl('Over', output_df$label), 'over',
                        ifelse(grepl('Under', output_df$label), 'under',
                               NA_character_
                               ))
    # get tidy line from the label
    num_part <- as.numeric(gsub('[A-Za-z| ]', '', output_df$label))
    output_df$tidyline <- num_part
    # get the tidy odds
    output_df$tidyamericanodds <- as.numeric(output_df$oddsAmerican)

  }

  # filter out the cols we don't need, i.e. not tidy ones
  names_to_keep <- names(output_df)[grepl('tidy|prop', names(output_df))]
  output_df <- output_df[, names(output_df) %in% names_to_keep]

  # stamp it up
  output_df$site <- 'draftkings'
  output_df$sport <- sport
  if (!'prop' %in% names(output_df)) {
    output_df$prop <- prop
  }

  # deliver
  return(output_df)

}


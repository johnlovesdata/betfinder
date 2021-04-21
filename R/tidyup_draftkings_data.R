tidyup_draftkings_data <- function(draftkings_data, sport, prop,
                                   key = get_key_path(sport = sport, prop = prop)) {

  # make the output using the input
  output_df <- draftkings_data

  # for each prop, append whatever tidy fields we can, which should make thte data useful across datasets
  if (prop %in% c('first team to score', 'ftts')) {

    # generate tidy names and odds
    output_df$tidyteam <- normalize_names(output_df$team, key = key)
    output_df$tidyopponent <- normalize_names(output_df$opponent, key = key)
    output_df$tidyamericanodds <- as.numeric(output_df$odds)

    # for flexible props, specify the value explicitly here
    output_df$prop <- 'first team to score'
  }

  if (prop %in% c('first player to score', 'fpts')) {

    # TODO: MAKE AN ACTUAL LOOKUP FOR PLAYERS
    # output_df$tidyplayer <- normalize_names(output_df$participant, key = key)
    # in the meantime, make a hacky field that should be consistent-ish across platforms
    output_df$tidyplayer <- hacky_tidyup_player_names(output_df$participant)
    # TODO: get player teams, hopefully in that same big-ass json of players? idk...
    output_df$tidyamericanodds <- as.numeric(output_df$oddsAmerican)

    # for flexible props, specify the value explicitly here
    output_df$prop <- 'first player to score'
  }

  # filter out the cols we don't need, i.e. not tidy ones
  names_to_keep <- names(output_df)[grepl('tidy|prop', names(output_df))]
  output_df <- output_df[, names(output_df) %in% names_to_keep]

  # stamp it up
  output_df$site <- 'draftkings'
  output_df$sport <- sport
  if (!'prop' %in% names(output_df)) {
    output$prop <- prop
  }

  # deliver
  return(output_df)

}

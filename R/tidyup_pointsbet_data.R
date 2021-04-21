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
    # TODO: MAKE AN ACTUAL LOOKUP FOR PLAYERS
    # output_df$tidyplayer <- normalize_names(output_df$participant, key = key)
    # in the meantime, make a hacky field that should be consistent-ish across platforms
    output_df$tidyplayer <- hacky_tidyup_player_names(output_df$name)
    # TODO: get player teams, hopefully in that same big-ass json of players? idk...
    ## odds are decimal
    output_df$tidyamericanodds <- ifelse(output_df$price - 1 < 1,
                                         -100 / (output_df$price - 1),
                                         (output_df$price - 1) * 100)
    # since prop arg is flexible, set it here for output
    output_df$prop <- 'first player to score'
  }

  # keep the tidy columns
  names_to_keep <- names(output_df)[grepl('tidy|prop', names(output_df))]
  output_df <- output_df[, names(output_df) %in% names_to_keep]

  # stamp it up
  output_df$site <- 'pointsbet'
  output_df$sport <- sport
  if (!'prop' %in% names(output_df)) {
    output$prop <- prop
  }

  return(output_df)
}

parse_fd_prop <- function(game_event, tab_name, prop_name = NULL, prop_regex = NULL, matchup, tipoff) {
  if (!tab_name %in% names(game_event)) return()
  tab_content <- game_event[[tab_name]]
  # extract attachments
  if (!'attachments' %in% names(tab_content)) return()
  tab_attachments <- tab_content$attachments
  # extract markets
  if (!'markets' %in% names(tab_attachments)) return()
  tab_markets <- tab_attachments$markets
  # identify bet markets
  if (!length(tab_markets) > 0) return()
    bet_markets <- do.call(rbind,
                           lapply(tab_markets, function(x)
                             data.frame(id = x[['marketId']], name = x[['marketName']])))
  # if there's just a single name for the prop, use prop_name
  if (!is.null(prop_name)) {
    if (!prop_name %in% bet_markets$name) return()
    market_id <- bet_markets$id[bet_markets$name == prop_name]
    # get the runners, which is where the bets live
    runners <- tab_markets[[market_id]]$runners
    # run through the runners list and get american odds by player
    runner_list <- lapply(runners, function(x) {
      data.frame(participant = x[['runnerName']],
                 handicap = x[['handicap']],
                 american_odds = x[['winRunnerOdds']][['americanDisplayOdds']][['americanOdds']])
    })
    # make a data.frame
    prop_df <- do.call(rbind, runner_list)
    prop_df$matchup <- matchup
    prop_df$tipoff <- tipoff
    return(prop_df)
  }
  if (!is.null(prop_regex)) {
    if (!any(grepl(prop_regex, bet_markets$name))) return()
    market_ids <- bet_markets$id[grepl(prop_regex, bet_markets$name)]
    # TODO: UPDATE THIS TO GRAB BOTH THE PLAYER/TEAM NAME AND THE PROP NAME, THEN UPDATE TIDYUP FUN
    if (length(market_ids) == 0) return()
    mkt_list <- list()
    for (i in market_ids) {
      mkt <- tab_markets[[i]]
      market_name <- mkt$marketName
      rnrs <- lapply(mkt$runners, function(x) {
        data.frame(name = x[['runnerName']],
                   prop = market_name,
                   handicap = x[['handicap']],
                   american_odds = x[['winRunnerOdds']][['americanDisplayOdds']][['americanOdds']])
      })
      if (length(rnrs) == 0) return()
      mkt_list[[length(mkt_list) + 1]] <- do.call(rbind, rnrs)
    }
  }
  prop_df <- do.call(rbind, mkt_list)
  if(inherits(prop_df, 'list')) return()
  prop_df$matchup <- matchup
  prop_df$tipoff <- tipoff
  return(prop_df)

}

parse_fd_game_lines <- function(game_event, matchup, tipoff, exclude_alts) {

  gl_outputs <- list()
  if (exclude_alts) {
    game_lines <- c('Moneyline', 'Total Points', 'Spread Betting')
  } else {
    game_lines <- c('Moneyline', 'Total Points', 'Spread Betting', 'Alternative Total Points', 'Alternative Spreads')
  }

  for (i in game_lines) {
    df <- parse_fd_prop(game_event, tab_name = 'main', prop_name = i, matchup = matchup, tipoff = tipoff)
    if (length(df) == 0) next
    df$Type <- i
    gl_outputs[[length(gl_outputs) + 1]] <- df
  }
  output_df <- do.call(rbind, gl_outputs)
  return(output_df)

}


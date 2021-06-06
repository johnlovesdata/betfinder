parse_fanduel_data <- function(fanduel_data, sport, prop) {

  # loop through fanduel_data and extract the correct prop
  output_list <- list()
  for (e in names(fanduel_data)) {
    # subset the game event
    game_event <- fanduel_data[[e]]
    matchup <- game_event$main$attachments$events[[e]]$name
    tipoff <- game_event$main$attachments$events[[e]]$openDate
    # extract correct props
    if (prop %in% c('first team to score', 'ftts')) {
      # get the right tab
      if (sport == 'mlb') tab_name <- 'hits_runs'
      if (sport == 'nba') tab_name <- '1st_quarter'
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = tab_name, prop_name = 'Team to Score First',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('first player to score', 'fpts')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'main', prop_name = 'First Basket',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player points alt', 'player pts alt')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'player_points', prop_regex = 'Alt Points',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player points ou', 'player pts ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'player_points', prop_regex = '- Points',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player points tiers', 'player pts tiers')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'player_points', prop_regex = 'To Score',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player assists alt', 'player asts alt')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'player_assists', prop_regex = 'Alt Assists',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player assists ou', 'player asts ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'player_assists', prop_regex = '- Assists',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player assists tiers', 'player asts tiers')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'player_assists', prop_regex = 'To Record',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player rebounds alt', 'player rebs alt')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'player_rebounds', prop_regex = 'Alt Rebounds',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player rebounds ou', 'player rebs ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'player_rebounds', prop_regex = '- Rebounds',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player rebounds tiers', 'player rebs tiers')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'player_rebounds', prop_regex = 'To Record',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player threes alt', 'player 3pts alt')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'player_threes', prop_regex = 'Alt 3-Pointers',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player threes ou', 'player 3pts ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'player_threes', prop_regex = '- Made Threes',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player threes tiers', 'player 3pts tiers')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'player_threes', prop_regex = '[1-9]\\+ Made Threes',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player most points')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'main', prop_name = 'Top Points Scorer',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player strikeouts ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'main', prop_regex = 'Strikeouts',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player hits tiers')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'hits_runs', prop_regex = 'To Record [1-9]',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    # if (prop %in% c('player hits tiers')) {
    #   output_list[[length(output_list) + 1]] <-
    #     parse_fd_prop(game_event = game_event, tab_name = 'hits_runs', prop_regex = 'To Record',
    #                   prop = prop, matchup = matchup, tipoff = tipoff)
    # }

    }
  # if output_list is empty, error, else return as a data.frame
  if (length(output_list) == 0) stop('no fanduel ', prop, ' props returned')
  output_df <- do.call(rbind, output_list)

  return(output_df)
  }


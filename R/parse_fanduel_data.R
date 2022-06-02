parse_fanduel_data <- function(fanduel_data, sport, prop = FALSE, game_lines = FALSE, exclude_live = TRUE, exclude_alts = FALSE, game_part = "full") {

  # loop through fanduel_data and extract the correct prop
  output_list <- list()
  for (e in names(fanduel_data)) {
    # subset the game event
    game_event <- fanduel_data[[e]]

    # nuke live games if specified, which is the default
    if (exclude_live) {
      status <- game_event$main$attachments$events[[e]]$inPlay
      if (status == TRUE) next
    }

    matchup <- game_event$main$attachments$events[[e]]$name
    tipoff <- game_event$main$attachments$events[[e]]$openDate

    if (game_lines == TRUE) {
      if (sport %in% c('nba', 'ncaaf', 'nfl', 'mlb', 'nhl')) {
        output_list[[length(output_list) + 1]] <-
          parse_fd_game_lines(game_event, matchup = matchup, tipoff = tipoff, exclude_alts = exclude_alts, game_part = game_part)
      }
    }

    # extract correct props
    if (prop %in% c('first team to score', 'ftts')) {
      # get the right tab
      if (sport == 'mlb') tab_name <- 'hits_runs'
      if (sport == 'nba') tab_name <- '1st_quarter'
      if (sport == 'nfl') tab_name <- 'main'
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = tab_name, prop_name = 'Team to Score First',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('fpts by team')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'first_basket', prop_name = 'First Team Basket Scorer',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('game go to ot', 'game go to overtime')) {
      # get the right tab
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'main', prop_name = 'Will there be OverTime?',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('first team to score q2')) {
      # get the right tab
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = '2nd_quarter', prop_name = '2nd Quarter Team to Score First',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('first team to score q3')) {
      # get the right tab
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = '3rd_quarter', prop_name = '3rd Quarter Team to Score First',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('first team to score q4')) {
      # get the right tab
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = '4th_quarter', prop_name = '4th Quarter Team to Score First',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('first player to score', 'fpts')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'main', prop_name = 'First Basket',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player points alt', 'player pts alt')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'player_points', prop_regex = 'Alt Points',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player points ou', 'player pts ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'player_points', prop_regex = '- Points',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player points tiers', 'player pts tiers')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'player_points', prop_regex = 'To Score',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player assists alt', 'player asts alt')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'player_assists', prop_regex = 'Alt Assists',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player assists ou', 'player asts ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'player_assists', prop_regex = '- Assists',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player assists tiers', 'player asts tiers')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'player_assists', prop_regex = 'To Record',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player rebounds alt', 'player rebs alt')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'player_rebounds', prop_regex = 'Alt Rebounds',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player rebounds ou', 'player rebs ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'player_rebounds', prop_regex = '- Rebounds',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player rebounds tiers', 'player rebs tiers')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'player_rebounds', prop_regex = 'To Record',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player threes alt', 'player 3pts alt')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'player_threes', prop_regex = 'Alt 3-Pointers',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player threes ou', 'player 3pts ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'player_threes', prop_regex = '- Made Threes',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player threes tiers', 'player 3pts tiers')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'player_threes', prop_regex = '[1-9]\\+ Made Threes',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player most points')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'main', prop_name = 'Top Points Scorer',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player double double')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'main', prop_name = 'To Record A Double Double',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player triple double')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'main', prop_name = 'To Record A Triple Double',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player strikeouts ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'main', prop_regex = ' - Strikeouts',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player strikeouts alt')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'main', prop_regex = '[^-] Strikeouts',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player hits tiers')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'hits_runs', prop_regex = 'To Record [1-9]',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player to hit home run', 'player to hit hr')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'hits_runs', prop_name = 'To Hit A Home Run',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player pts+reb+ast ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'player_combos', prop_regex = ' - Pts \\+ Reb \\+ Ast$',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player pts+reb ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'player_combos', prop_regex = ' - Pts \\+ Reb$',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player pts+ast ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'player_combos', prop_regex = ' - Pts \\+ Ast$',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player reb+ast ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'player_combos', prop_regex = ' - Reb \\+ Ast$',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player rush atts ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'player_props', prop_regex = ' - Rush Attempts',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player rush yds ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'player_props', prop_regex = ' - Rushing Yds',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player recs ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'player_props', prop_regex = ' - Total Receptions$',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player rec yds ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'player_props', prop_regex = ' - Receiving Yds$',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player rush+rec yds ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'player_props', prop_regex = ' - Rushing \\+ Receiving Yds',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player pass atts ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'player_props', prop_regex = 'Pass Attempts',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player pass yds ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'player_props', prop_regex = ' - Passing Yds',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player pass tds ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'player_props', prop_regex = 'Passing TD',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player any td')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'player_props', prop_name = 'Any Time Touchdown Scorer',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player first td')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'player_props', prop_name = 'First Touchdown Scorer',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player goals', 'goals', 'goalscorer', 'player goals ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'player_goals', prop_regex = ' - Goals',
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player shots ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_fd_prop(game_event = game_event, tab_name = 'player_shots', prop_regex = ' - Shots',
                      matchup = matchup, tipoff = tipoff)
    }
  }

  # if output_list is empty, error, else return as a data.frame
  if (length(output_list) == 0) stop('no fanduel ', prop, ' props returned')
  output_df <- dplyr::bind_rows(output_list)
  #output_df <- output_df[!is.na(output_df$participant), ]
  output_df <- output_df[complete.cases(output_df), ]
  return(output_df)
  }


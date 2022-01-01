parse_betrivers_data <- function(betrivers_data, sport, prop = FALSE, game_lines = FALSE) {

  # loop through betrivers_data and extract the correct prop
  output_list <- list()
  for (e in names(betrivers_data)) {
    # subset the game event
    game_event <- betrivers_data[[e]]
    matchup <- game_event$name
    tipoff <- game_event$start

    if (game_lines == TRUE) {
      browser()
      gl_out <- parse_br_main(game_event = game_event, matchup = matchup, tipoff = tipoff)
      output_list[[length(output_list) + 1]] <-
        parse_br_main(game_event = game_event, matchup = matchup, tipoff = tipoff)
      next
    }

    # extract correct props
    if (prop %in% c('first team to score', 'ftts')) {
      output_list[[length(output_list) + 1]] <-
        parse_br_prop(game_event = game_event, category_name = 'Game', prop_name = "Next Team to Score - at Score 0-0",
                      matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('first player to score', 'fpts')) {
      output_list[[length(output_list) + 1]] <-
        parse_br_prop(game_event = game_event, category_name = 'Player Specials', prop_name = 'Player to Score the First Field Goal of the Game',
                      matchup = matchup, tipoff = tipoff)
    }

  #   if (prop %in% c('player points alt', 'player pts alt')) {
  #     output_list[[length(output_list) + 1]] <-
  #       parse_br_prop(game_event = game_event, category_name = 'player_points', prop_regex = 'Alt Points',
  #                     matchup = matchup, tipoff = tipoff)
  #   }
  #
  #   if (prop %in% c('player points ou', 'player pts ou')) {
  #     output_list[[length(output_list) + 1]] <-
  #       parse_br_prop(game_event = game_event, category_name = 'player_points', prop_regex = '- Points',
  #                     matchup = matchup, tipoff = tipoff)
  #   }
  #   if (prop %in% c('player points tiers', 'player pts tiers')) {
  #     output_list[[length(output_list) + 1]] <-
  #       parse_br_prop(game_event = game_event, category_name = 'player_points', prop_regex = 'To Score',
  #                     matchup = matchup, tipoff = tipoff)
  #   }
  #   if (prop %in% c('player assists alt', 'player asts alt')) {
  #     output_list[[length(output_list) + 1]] <-
  #       parse_br_prop(game_event = game_event, category_name = 'player_assists', prop_regex = 'Alt Assists',
  #                     matchup = matchup, tipoff = tipoff)
  #   }
  #   if (prop %in% c('player assists ou', 'player asts ou')) {
  #     output_list[[length(output_list) + 1]] <-
  #       parse_br_prop(game_event = game_event, category_name = 'player_assists', prop_regex = '- Assists',
  #                     matchup = matchup, tipoff = tipoff)
  #   }
  #   if (prop %in% c('player assists tiers', 'player asts tiers')) {
  #     output_list[[length(output_list) + 1]] <-
  #       parse_br_prop(game_event = game_event, category_name = 'player_assists', prop_regex = 'To Record',
  #                     matchup = matchup, tipoff = tipoff)
  #   }
  #   if (prop %in% c('player rebounds alt', 'player rebs alt')) {
  #     output_list[[length(output_list) + 1]] <-
  #       parse_br_prop(game_event = game_event, category_name = 'player_rebounds', prop_regex = 'Alt Rebounds',
  #                     matchup = matchup, tipoff = tipoff)
  #   }
  #   if (prop %in% c('player rebounds ou', 'player rebs ou')) {
  #     output_list[[length(output_list) + 1]] <-
  #       parse_br_prop(game_event = game_event, category_name = 'player_rebounds', prop_regex = '- Rebounds',
  #                     matchup = matchup, tipoff = tipoff)
  #   }
  #   if (prop %in% c('player rebounds tiers', 'player rebs tiers')) {
  #     output_list[[length(output_list) + 1]] <-
  #       parse_br_prop(game_event = game_event, category_name = 'player_rebounds', prop_regex = 'To Record',
  #                     matchup = matchup, tipoff = tipoff)
  #   }
  #   if (prop %in% c('player threes alt', 'player 3pts alt')) {
  #     output_list[[length(output_list) + 1]] <-
  #       parse_br_prop(game_event = game_event, category_name = 'player_threes', prop_regex = 'Alt 3-Pointers',
  #                     matchup = matchup, tipoff = tipoff)
  #   }
  #   if (prop %in% c('player threes ou', 'player 3pts ou')) {
  #     output_list[[length(output_list) + 1]] <-
  #       parse_br_prop(game_event = game_event, category_name = 'player_threes', prop_regex = '- Made Threes',
  #                     matchup = matchup, tipoff = tipoff)
  #   }
  #   if (prop %in% c('player threes tiers', 'player 3pts tiers')) {
  #     output_list[[length(output_list) + 1]] <-
  #       parse_br_prop(game_event = game_event, category_name = 'player_threes', prop_regex = '[1-9]\\+ Made Threes',
  #                     matchup = matchup, tipoff = tipoff)
  #   }
  #   if (prop %in% c('player most points')) {
  #     output_list[[length(output_list) + 1]] <-
  #       parse_br_prop(game_event = game_event, category_name = 'main', prop_name = 'Top Points Scorer',
  #                     matchup = matchup, tipoff = tipoff)
  #   }
  #   if (prop %in% c('player double double')) {
  #     output_list[[length(output_list) + 1]] <-
  #       parse_br_prop(game_event = game_event, category_name = 'Player Specials', prop_name = 'To record a double-double',
  #                     matchup = matchup, tipoff = tipoff)
  #   }
  #   if (prop %in% c('player triple double')) {
  #     output_list[[length(output_list) + 1]] <-
  #       parse_br_prop(game_event = game_event, category_name = 'main', prop_name = 'To Record A Triple Double',
  #                     matchup = matchup, tipoff = tipoff)
  #   }
  #   if (prop %in% c('player strikeouts ou')) {
  #     output_list[[length(output_list) + 1]] <-
  #       parse_br_prop(game_event = game_event, category_name = 'main', prop_regex = ' - Strikeouts',
  #                     matchup = matchup, tipoff = tipoff)
  #   }
  #   if (prop %in% c('player strikeouts alt')) {
  #     output_list[[length(output_list) + 1]] <-
  #       parse_br_prop(game_event = game_event, category_name = 'main', prop_regex = '[^-] Strikeouts',
  #                     matchup = matchup, tipoff = tipoff)
  #   }
  #   if (prop %in% c('player hits tiers')) {
  #     output_list[[length(output_list) + 1]] <-
  #       parse_br_prop(game_event = game_event, category_name = 'hits_runs', prop_regex = 'To Record [1-9]',
  #                     matchup = matchup, tipoff = tipoff)
  #   }
  #   if (prop %in% c('player to hit home run', 'player to hit hr')) {
  #     output_list[[length(output_list) + 1]] <-
  #       parse_br_prop(game_event = game_event, category_name = 'hits_runs', prop_name = 'To Hit A Home Run',
  #                     matchup = matchup, tipoff = tipoff)
  #   }
  #   if (prop %in% c('player pts+reb+ast ou')) {
  #     output_list[[length(output_list) + 1]] <-
  #       parse_br_prop(game_event = game_event, category_name = 'player_combos', prop_regex = ' - Pts \\+ Reb \\+ Ast$',
  #                     matchup = matchup, tipoff = tipoff)
  #   }
  #   if (prop %in% c('player pts+reb ou')) {
  #     output_list[[length(output_list) + 1]] <-
  #       parse_br_prop(game_event = game_event, category_name = 'player_combos', prop_regex = ' - Pts \\+ Reb$',
  #                     matchup = matchup, tipoff = tipoff)
  #   }
  #   if (prop %in% c('player pts+ast ou')) {
  #     output_list[[length(output_list) + 1]] <-
  #       parse_br_prop(game_event = game_event, category_name = 'player_combos', prop_regex = ' - Pts \\+ Ast$',
  #                     matchup = matchup, tipoff = tipoff)
  #   }
  #   if (prop %in% c('player reb+ast ou')) {
  #     output_list[[length(output_list) + 1]] <-
  #       parse_br_prop(game_event = game_event, category_name = 'player_combos', prop_regex = ' - Reb \\+ Ast$',
  #                     matchup = matchup, tipoff = tipoff)
  #   }
  #   if (prop %in% c('player rush atts ou')) {
  #     output_list[[length(output_list) + 1]] <-
  #       parse_br_prop(game_event = game_event, category_name = 'player_props', prop_regex = ' - Rush Attempts',
  #                     matchup = matchup, tipoff = tipoff)
  #   }
  #   if (prop %in% c('player rush yds ou')) {
  #     output_list[[length(output_list) + 1]] <-
  #       parse_br_prop(game_event = game_event, category_name = 'player_props', prop_regex = ' - Rushing Yds',
  #                     matchup = matchup, tipoff = tipoff)
  #   }
  #   if (prop %in% c('player recs ou')) {
  #     output_list[[length(output_list) + 1]] <-
  #       parse_br_prop(game_event = game_event, category_name = 'player_props', prop_regex = ' - Total Receptions$',
  #                     matchup = matchup, tipoff = tipoff)
  #   }
  #   if (prop %in% c('player rec yds ou')) {
  #     output_list[[length(output_list) + 1]] <-
  #       parse_br_prop(game_event = game_event, category_name = 'player_props', prop_regex = ' - Receiving Yds$',
  #                     matchup = matchup, tipoff = tipoff)
  #   }
  #   if (prop %in% c('player rush+rec yds ou')) {
  #     output_list[[length(output_list) + 1]] <-
  #       parse_br_prop(game_event = game_event, category_name = 'player_props', prop_regex = ' - Rushing \\+ Receiving Yds',
  #                     matchup = matchup, tipoff = tipoff)
  #   }
  #   if (prop %in% c('player pass atts ou')) {
  #     output_list[[length(output_list) + 1]] <-
  #       parse_br_prop(game_event = game_event, category_name = 'player_props', prop_regex = 'Pass Attempts',
  #                     matchup = matchup, tipoff = tipoff)
  #   }
  #   if (prop %in% c('player pass yds ou')) {
  #     output_list[[length(output_list) + 1]] <-
  #       parse_br_prop(game_event = game_event, category_name = 'player_props', prop_regex = ' - Passing Yds',
  #                     matchup = matchup, tipoff = tipoff)
  #   }
  #   if (prop %in% c('player pass tds ou')) {
  #     output_list[[length(output_list) + 1]] <-
  #       parse_br_prop(game_event = game_event, category_name = 'player_props', prop_regex = 'Passing TD',
  #                     matchup = matchup, tipoff = tipoff)
  #   }
  #   if (prop %in% c('player any td')) {
  #     output_list[[length(output_list) + 1]] <-
  #       parse_br_prop(game_event = game_event, category_name = 'player_props', prop_name = 'Any Time Touchdown Scorer',
  #                     matchup = matchup, tipoff = tipoff)
  #   }
  #   if (prop %in% c('player first td')) {
  #     output_list[[length(output_list) + 1]] <-
  #       parse_br_prop(game_event = game_event, category_name = 'player_props', prop_name = 'First Touchdown Scorer',
  #                     matchup = matchup, tipoff = tipoff)
  #   }

  }

  # if output_list is empty, error, else return as a data.frame
  if (length(output_list) == 0) stop('no betrivers ', prop, ' props returned')
  output_df <- do.call(rbind, output_list)

  return(output_df)
}


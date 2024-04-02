parse_draftkings_data <- function(draftkings_data, sport, prop = FALSE, game_lines = FALSE, exclude_live = TRUE, offer_category = 'Game Lines', gl_subgroups = c('Game')) {

  output_list <- list()

  for (e in names(draftkings_data)) {
    game_event <- draftkings_data[[e]]

    # nuke live games if specified, which is the default
    if (exclude_live) {
      status <- game_event$event$eventStatus$state
      if (status == "STARTED") next
    }

    # get matchup name and start time
    matchup <- game_event$event$name
    tipoff <- game_event$event$startDate

    # break out the offer markets, always necessary
    offer_categories <- game_event$eventCategories
    offer_category_names <- unlist(lapply(offer_categories, '[[', 'name'))
    if (game_lines == TRUE) {
      if (sport %in% c('nba', 'ncaaf', 'nfl', 'mlb', 'nhl')) {
        output_list[[length(output_list) + 1]] <-
          parse_dk_game_lines(offer_categories, offer_category = offer_category, gl_subgroups = gl_subgroups, matchup = matchup, tipoff = tipoff)
      }
    }
    if (is.null(prop)) {
      next
    }
    if (prop %in% c('first team to score', 'ftts')) {
      if (sport %in% c('nba', 'ncaaf', 'nfl')) {
        output_list[[length(output_list) + 1]] <-
          parse_dk_prop(offer_categories, prop_group = 'Game Props', prop_subgroup = 'First to Score', prop_name = '1st to Score',
                        prop = prop, matchup = matchup, tipoff = tipoff)
      }
      if (sport == 'mlb') {
        output_list[[length(output_list) + 1]] <-
          parse_dk_prop(offer_categories, prop_group = 'Game Props', prop_subgroup = 'First/Last Run', prop_name = '1st Run',
                        prop = prop, matchup = matchup, tipoff = tipoff)
      }
    }
    if (prop %in% c('game go to overtime', 'game go to ot')) {
      output_list[[length(output_list) + 1]] <- parse_dk_prop(offer_categories, prop_group = 'Game Props', prop_subgroup = 'To go to OT', prop_name = 'Overtime', prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('first player to score', 'fpts')) {
      output_list[[length(output_list) + 1]] <- parse_dk_prop(offer_categories, prop_group = '1st Basket Props', prop_subgroup = 'First FG', prop_name = 'First Field Goal', prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player points ou', 'player pts ou')) {
      output_list[[length(output_list) + 1]] <- parse_dk_prop(offer_categories, prop_group = 'Player Points', prop_subgroup = 'Points', prop_regex = ' Points$', prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player assists ou', 'player asts ou')) {
      output_list[[length(output_list) + 1]] <- parse_dk_prop(offer_categories, prop_group = 'Player Assists', prop_subgroup = 'Assists', prop_regex = ' Assists$', prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player rebounds ou', 'player rebs ou')) {
      output_list[[length(output_list) + 1]] <- parse_dk_prop(offer_categories, prop_group = 'Player Rebounds', prop_subgroup = 'Rebounds', prop_regex = ' Rebounds$', prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player pts-rebs-asts ou')) {
      output_list[[length(output_list) + 1]] <- parse_dk_prop(offer_categories, prop_group = 'Player Combos', prop_subgroup = 'Pts + Reb + Ast', prop_regex = ' Points \\+ Assists \\+ Rebounds$', prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player pts-rebs ou')) {
      output_list[[length(output_list) + 1]] <- parse_dk_prop(offer_categories, prop_group = 'Player Combos', prop_subgroup = 'Pts + Reb', prop_regex = ' Points \\+ Rebounds$', prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player pts-asts ou')) {
      output_list[[length(output_list) + 1]] <- parse_dk_prop(offer_categories, prop_group = 'Player Combos', prop_subgroup = 'Pts + Ast', prop_regex = ' Points \\+ Assists$', prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player rebs-asts ou')) {
      output_list[[length(output_list) + 1]] <- parse_dk_prop(offer_categories, prop_group = 'Player Combos', prop_subgroup = 'Ast + Reb', prop_regex = ' Assists \\+ Rebounds$', prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player three-pointers ou', 'player 3pts ou')) {
      output_list[[length(output_list) + 1]] <- parse_dk_prop(offer_categories, prop_group = 'Player Threes', prop_subgroup = 'Threes', prop_regex = ' Three Pointers Made$', prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player blocks ou')) {
      output_list[[length(output_list) + 1]] <- parse_dk_prop(offer_categories, prop_group = 'Player Blocks/Steals', prop_subgroup = 'Blocks ', prop_regex = ' Blocks$', prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player steals ou')) {
      output_list[[length(output_list) + 1]] <- parse_dk_prop(offer_categories, prop_group = 'Player Blocks/Steals', prop_subgroup = 'Steals ', prop_regex = ' Steals$', prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player turnovers ou', 'player to ou')) {
      output_list[[length(output_list) + 1]] <- parse_dk_prop(offer_categories, prop_group = 'Player Turnovers', prop_subgroup = 'Turnovers', prop_regex = ' Turnovers$', prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player steals plus blocks ou')) {
      output_list[[length(output_list) + 1]] <- parse_dk_prop(offer_categories, prop_group = 'Player Blocks/Steals', prop_subgroup = 'Steals + Blocks', prop_regex = ' Steals \\+ Blocks$', prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player most points')) {
      output_list[[length(output_list) + 1]] <- parse_dk_prop(offer_categories, prop_group = 'Player Points', prop_subgroup = 'Leading Scorer', prop_name = 'Leading Scorer of the Game', prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player double double')) {
      output_list[[length(output_list) + 1]] <- parse_dk_prop(offer_categories, prop_group = 'Player Combos', prop_subgroup = 'Double-Double', prop_regex = ' Double-Double$', prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player triple double')) {
      output_list[[length(output_list) + 1]] <- parse_dk_prop(offer_categories, prop_group = 'Player Combos', prop_subgroup = 'Triple-Double', prop_regex = ' Triple-Double$', prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player strikeouts ou', 'strikeouts')) {
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'Pitcher Props', prop_subgroup = 'Strikeouts Thrown', prop_regex = 'Strikeouts',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('pitcher walks ou', 'walks')) {
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'Pitcher Props', prop_subgroup = 'Walks Allowed', prop_regex = 'Walks',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('pitcher outs ou', 'outs')) {
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'Pitcher Props', prop_subgroup = 'Outs Recorded', prop_regex = 'Outs',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('pitcher hits allowed ou', 'outs')) {
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'Pitcher Props', prop_subgroup = 'Hits Allowed', prop_regex = 'Hits',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('pitcher earned runs ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'Pitcher Props', prop_subgroup = 'Earned Runs Allowed', prop_regex = 'Earned Runs',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player hits ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'Batter Props', prop_subgroup = 'Hits', prop_regex = 'Hits',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player total bases ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'Batter Props', prop_subgroup = 'Total Bases', prop_regex = 'Total Bases',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player stolen bases ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'Batter Props', prop_subgroup = 'Stolen Bases', prop_regex = 'Stolen Bases',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player rbis ou', 'rbis')) {
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'Batter Props', prop_subgroup = 'RBIs', prop_regex = 'RBIs',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player runs ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'Batter Props', prop_subgroup = 'Runs Scored', prop_regex = 'Runs',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player hrs ou', 'player home runs ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'Batter Props', prop_subgroup = 'Home Runs', prop_regex = 'Home Run',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player rush atts ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'Rush/Rec Props', prop_subgroup = 'Rush Attempts', prop_regex = 'Rushing Attempts',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player rush yds ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'Rush/Rec Props', prop_subgroup = 'Rush Yds', prop_regex = 'Rushing Yards',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player recs ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'Rush/Rec Props', prop_subgroup = 'Receptions', prop_regex = 'Receptions',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player rec yds ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'Rush/Rec Props', prop_subgroup = 'Rec Yds', prop_regex = 'Receiving Yards',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player rush+rec yds ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'Rush/Rec Props', prop_subgroup = 'Rush + Rec Yds', prop_regex = 'Rushing \\+ Receiving Yards',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player pass atts ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'Passing Props', prop_subgroup = 'Pass Attempts', prop_regex = 'Passing Attempts',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player pass yds ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'Passing Props', prop_subgroup = 'Pass Yds', prop_regex = 'Passing Yards',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player pass tds ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'Passing Props', prop_subgroup = 'Pass TDs', prop_regex = 'Passing Touchdowns',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player any td')) {
      # first/last/any tds are all in one big data.frame, which gets re-labeled correctly in tidyup_draftkings_data()
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'TD Scorers', prop_subgroup = 'TD Scorer', prop_name = 'Anytime TD Scorer',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player first td')) {
      # first/last/any tds are all in one big data.frame, which gets re-labeled correctly in tidyup_draftkings_data()
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'TD Scorers', prop_subgroup = 'TD Scorer', prop_name = 'First TD Scorer',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('goalscorer', 'goals', 'player any goal')) {
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'Goalscorer', prop_subgroup = 'Anytime Goalscorer', prop_regex = '',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('shots')) {
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'Shots on Goal', prop_subgroup = 'Player Shots on Goal', prop_regex = 'Shots on Goal',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('assists')) {
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'Player Props', prop_subgroup = 'Assists', prop_regex = 'Assists',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('blocked shots')) {
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'Player Props', prop_subgroup = 'Blocked Shots', prop_regex = 'Blocked Shots',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('points')) {
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'Player Props', prop_subgroup = 'Points', prop_regex = 'Points',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('goalie saves')) {
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'Goalie Props', prop_subgroup = 'Saves', prop_regex = 'Saves',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('fg made')) {
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'D/ST Props', prop_subgroup = 'FG Made', prop_regex = 'Field Goal Made',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('kicking pts')) {
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'D/ST Props', prop_subgroup = 'Kicking Pts', prop_regex = 'Kicking Points',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('pat made')) {
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'D/ST Props', prop_subgroup = 'PAT Made', prop_regex = 'Extra Point Made',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }

  }

    # if output_list is empty, error
    if (!'output_list' %in% ls()) stop('no draftkings ', prop, ' returned')
    if (length(output_list) == 0) stop('no draftkings ', prop, ' returned')
    output_df <- dplyr::bind_rows(output_list)
    output_df$prop <- prop

    return(output_df)
}






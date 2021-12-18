parse_draftkings_data <- function(draftkings_data, sport, prop = FALSE, game_lines = FALSE) {

  output_list <- list()

  for (e in names(draftkings_data)) {
    game_event <- draftkings_data[[e]]
    # get matchup name and start time
    matchup <- game_event$event$name
    tipoff <- game_event$event$startDate

    # break out the offer markets, always necessary
    offer_categories <- game_event$eventCategories
    offer_category_names <- unlist(lapply(offer_categories, '[[', 'name'))
    if (game_lines == TRUE) {
      if (sport %in% c('nba', 'ncaaf', 'nfl', 'mlb')) {
        output_list[[length(output_list) + 1]] <-
          parse_dk_main(offer_categories, matchup = matchup, tipoff = tipoff)
      }
    }
    if (is.null(prop)) {
      next
    }
    if (prop %in% c('first team to score', 'ftts')) {
      if (sport %in% c('nba', 'ncaaf', 'nfl')) {
        output_list[[length(output_list) + 1]] <-
          parse_dk_prop(offer_categories, prop_group = 'Game Props', prop_subgroup = '1st/Last to Score', prop_name = '1st to Score',
                        prop = prop, matchup = matchup, tipoff = tipoff)
      }
      if (sport == 'mlb') {
        output_list[[length(output_list) + 1]] <-
          parse_dk_prop(offer_categories, prop_group = 'Game Props', prop_subgroup = 'First/Last Run', prop_name = '1st Run',
                        prop = prop, matchup = matchup, tipoff = tipoff)
      }
    }
    if (prop %in% c('first player to score', 'fpts')) {
      output_list[[length(output_list) + 1]] <- parse_dk_prop(offer_categories, prop_group = 'Player Props', prop_subgroup = 'First FG', prop_name = 'First Field Goal', prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player points ou', 'player pts ou')) {
      output_list[[length(output_list) + 1]] <- parse_dk_prop(offer_categories, prop_group = 'Player Props', prop_subgroup = 'Points', prop_regex = ' Points$', prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player assists ou', 'player asts ou')) {
      output_list[[length(output_list) + 1]] <- parse_dk_prop(offer_categories, prop_group = 'Player Props', prop_subgroup = 'Assists', prop_regex = ' Assists$', prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player rebounds ou', 'player rebs ou')) {
      output_list[[length(output_list) + 1]] <- parse_dk_prop(offer_categories, prop_group = 'Player Props', prop_subgroup = 'Rebounds', prop_regex = ' Rebounds$', prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player three-pointers ou', 'player 3pts ou')) {
      output_list[[length(output_list) + 1]] <- parse_dk_prop(offer_categories, prop_group = 'Player Props', prop_subgroup = 'Threes', prop_regex = ' Three Pointers Made$', prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player blocks ou')) {
      output_list[[length(output_list) + 1]] <- parse_dk_prop(offer_categories, prop_group = 'Player Props', prop_subgroup = 'Blocks', prop_regex = ' Blocks$', prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player steals ou')) {
      output_list[[length(output_list) + 1]] <- parse_dk_prop(offer_categories, prop_group = 'Player Props', prop_subgroup = 'Steals', prop_regex = ' Steals$', prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player turnovers ou', 'player to ou')) {
      output_list[[length(output_list) + 1]] <- parse_dk_prop(offer_categories, prop_group = 'Player Props', prop_subgroup = 'Turnovers', prop_regex = ' Turnovers$', prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player most points')) {
      output_list[[length(output_list) + 1]] <- parse_dk_prop(offer_categories, prop_group = 'Player Props', prop_subgroup = 'Top Point Scorer', prop_name = 'Leading Scorer of the Game', prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player double double')) {
      output_list[[length(output_list) + 1]] <- parse_dk_prop(offer_categories, prop_group = 'Player Props', prop_subgroup = 'Double-Double', prop_regex = ' Double-Double$', prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player triple double')) {
      output_list[[length(output_list) + 1]] <- parse_dk_prop(offer_categories, prop_group = 'Player Props', prop_subgroup = 'Triple-Double', prop_regex = ' Triple-Double$', prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player strikeouts ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'Player Props', prop_subgroup = 'Strikeouts by Pitcher', prop_regex = 'Strikeouts Thrown',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player hits ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'Player Props', prop_subgroup = 'Hits', prop_regex = 'Hits',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player rbis ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'Player Props', prop_subgroup = 'RBIs', prop_regex = 'RBIs',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player runs ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'Player Props', prop_subgroup = 'Runs Scored', prop_regex = 'Runs',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player hrs ou', 'player home runs ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'Player Props', prop_subgroup = 'Home Runs', prop_regex = 'Home Run',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player rush atts ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'RB/WR Props', prop_subgroup = 'Rush Attempts', prop_regex = 'Rushing Attempts',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player rush yds ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'RB/WR Props', prop_subgroup = 'Rush Yds', prop_regex = 'Rushing Yards',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player recs ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'RB/WR Props', prop_subgroup = 'Receptions', prop_regex = 'Receptions',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player rec yds ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'RB/WR Props', prop_subgroup = 'Rec Yds', prop_regex = 'Receiving Yards',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player rush+rec yds ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'RB/WR Props', prop_subgroup = 'Rush + Rec Yds', prop_regex = 'Rushing \\+ Receiving Yards',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player pass atts ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'QB Props', prop_subgroup = 'Pass Attempts', prop_regex = 'Passing Attempts',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player pass yds ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'QB Props', prop_subgroup = 'Pass Yds', prop_regex = 'Passing Yards',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player pass tds ou')) {
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'QB Props', prop_subgroup = 'Pass TDs', prop_regex = 'Passing Touchdowns',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('player any td', 'player first td')) {
      # first/last/any tds are all in one big data.frame, which gets re-labeled correctly in tidyup_draftkings_data()
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'TD Scorers', prop_subgroup = 'TD Scorer', prop_regex = 'Touchdown Scorer',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('goalscorer', 'goals', 'player first goal', 'player any goal', 'player last goal')) {
      # first/last/any tds are all in one big data.frame, which gets re-labeled correctly in tidyup_draftkings_data()
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'Player Props', prop_subgroup = 'Goalscorer', prop_regex = 'Goalscorer',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('shots')) {
      # first/last/any tds are all in one big data.frame, which gets re-labeled correctly in tidyup_draftkings_data()
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'Player Props', prop_subgroup = 'Shots on Goal', prop_regex = 'Shots',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('assists')) {
      # first/last/any tds are all in one big data.frame, which gets re-labeled correctly in tidyup_draftkings_data()
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'Player Props', prop_subgroup = 'Assists', prop_regex = 'Assists',
                      prop = prop, matchup = matchup, tipoff = tipoff)
    }
    if (prop %in% c('points')) {
      # first/last/any tds are all in one big data.frame, which gets re-labeled correctly in tidyup_draftkings_data()
      output_list[[length(output_list) + 1]] <-
        parse_dk_prop(offer_categories, prop_group = 'Player Props', prop_subgroup = 'Points', prop_regex = 'Points',
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






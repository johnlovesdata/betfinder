parse_fanduel_data <- function(fanduel_data, prop) {

  # loop through fanduel_data and extract the correct prop
  output_list <- list()
  for (e in names(fanduel_data)) {

    game_event <- fanduel_data[[e]]

    # extract main event
    if ('main' %in% names(game_event)) main <- game_event$main else next
    # extract attachments
    if ('attachments' %in% names(main)) main_attachments <- main$attachments else next
    # extract matchup
    if ('events' %in% names(main_attachments)) matchup <- main_attachments$events[[e]]$name else next
    # extract markets
    if ('markets' %in% names(main_attachments)) main_markets <- main_attachments$markets else next
    # identify bet markets
    if (length(main_markets) > 0) {
      main_bet_markets <- do.call(rbind, lapply(main_markets, function(x) data.frame(id = x[['marketId']], name = x[['marketName']])))
    } else {
      next
    }
    # extract correct props

    if (prop %in% c('first team to score', 'ftts')) {

      # get the first quarter props
      if ('first_quarter' %in% names(game_event)) first_quarter <- game_event$first_quarter else next
      # extract attachments
      if ('attachments' %in% names(first_quarter)) first_quarter_attachments <- first_quarter$attachments else next
      # extract markets
      if ('markets' %in% names(first_quarter_attachments)) first_quarter_markets <- first_quarter_attachments$markets else next
      # identify bet markets
      if (length(first_quarter_markets) > 0) {
        first_quarter_bet_markets <-
          do.call(rbind, lapply(first_quarter_markets, function(x) data.frame(id = x[['marketId']], name = x[['marketName']])))
      } else {
        next
      }
      # get the market id of the bet
      if ('Team to Score First' %in% first_quarter_bet_markets$name) {
        market_id <- first_quarter_bet_markets$id[first_quarter_bet_markets$name == 'Team to Score First']
        } else {
          next
        }
      # get the market of the bet
      if (market_id %in% names(first_quarter_markets)) market <- first_quarter_markets[[market_id]] else next
      # get the runners, which is where the bets live
      runners <- market$runners
      # run through the runners list and get american odds by player
      ftts_list <- lapply(runners, function(x) {
        data.frame(team = x[['runnerName']],
                   american_odds = x[['winRunnerOdds']][['americanDisplayOdds']][['americanOdds']])
      })

      # make a data.frame
      ftts <- do.call(rbind, ftts_list)
      ftts$matchup <- matchup

      # stash in the output_list
      output_list[[length(output_list) + 1]] <- ftts
    }
    if (prop %in% c('first player to score', 'fpts')) {

      # get the market id of the bet
      if ('First Basket' %in% main_bet_markets$name) market_id <- main_bet_markets$id[main_bet_markets$name == 'First Basket'] else next
      # get the market of the bet
      if (market_id %in% names(main_markets)) market <- main_markets[[market_id]] else next
      # get the runners, which is where the bets live
      runners <- market$runners
      # run through the runners list and get american odds by player
      fpts_list <- lapply(runners, function(x) {
        data.frame(player = x[['runnerName']],
                   american_odds = x[['winRunnerOdds']][['americanDisplayOdds']][['americanOdds']])
      })

      fpts <- do.call(rbind, fpts_list)
      fpts$matchup <- matchup

      # stash in the output_list
      output_list[[length(output_list) + 1]] <- fpts
    }

    if (prop %in% c('player points alt', 'player points ou', 'player points tiers',
                    'player pts alt', 'player pts ou', 'player pts tiers')) {

      # skip if no player points props available
      prop <- gsub(' pts ', 'points ', prop)
      market_label <- 'Player Points'
      if (!market_label %in% unique(game_event_market_groups$name)) {
        next
      } else {
        player_points <-
          game_event_market_groups$markets[game_event_market_groups$name == market_label][[1]]
      }

      # get correct props
      prop_type <- ifelse(grepl('alt', prop), 'points alt',
                   ifelse(grepl('ou', prop), 'points ou',
                   ifelse(grepl('tiers', prop), 'points tiers',
                          NA_character_
                   )))

      # handle the different kinds of player points bets
      if (prop_type == 'points alt') {

        if (length(player_points$name[grepl('Alt', player_points$name)]) < 1) {
          next
          } else {
          alt_props <- player_points[grepl('Alt', player_points$name), ]
          points_df <- do.call(rbind, alt_props$selections)
          }
        }

      if (prop_type == 'points ou') {
          if (length(player_points$name[grepl('- Points', player_points$name)]) < 1) {
            next
          } else {
            ou_props <- player_points[grepl('- Points', player_points$name), ]
            points_df <- do.call(rbind, ou_props$selections)
          }
      }

      if (prop_type == 'points tiers') {
        if (length(player_points$name[grepl('To Score', player_points$name)]) < 1) {
          next
        } else {
          thresh_props <- player_points[grepl('To Score', player_points$name), ]
        }
        # gotta label the points values somehow, which is to use the name of the field, requiring a loop i think
        points_list <- list()
        for (n  in 1:nrow(thresh_props)) {
          selections <- thresh_props$selections[[n]]
          if (nrow(selections) < 1) {
            next
          }  else {
            selections$prop_details <- thresh_props$name[[n]]
            points_list[[length(points_list) + 1]] <- selections
          }
        }
        points_df <- do.call(rbind, points_list)
      }

      # extract the description, which has the matchup, and stash in the output_list
      points_df$description <- game_event$externaldescription
      output_list[[length(output_list) + 1]] <- points_df
    }

    if (prop %in% c('player rebounds alt', 'player rebounds ou', 'player rebounds tiers',
                    'player rebs alt', 'player rebs ou', 'player rebs tiers')) {

      # skip if no player rebounds props available
      prop <- gsub(' rebs ', ' rebounds ', prop)
      market_label <- 'Player Rebounds'
      if (!market_label %in% unique(game_event_market_groups$name)) {
        next
      } else {
        player_rebounds <-
          game_event_market_groups$markets[game_event_market_groups$name == market_label][[1]]
      }

      # get correct props
      prop_type <- ifelse(grepl('alt$', prop), 'rebounds alt',
                   ifelse(grepl('ou$', prop), 'rebounds ou',
                   ifelse(grepl('tiers$', prop), 'rebounds tiers',
                          NA_character_
                                 )))

      # handle the different kinds of player rebounds bets
      if (prop_type == 'rebounds alt') {

        if (length(player_rebounds$name[grepl('Alt', player_rebounds$name)]) < 1) {
          next
        } else {
          alt_props <- player_rebounds[grepl('Alt', player_rebounds$name), ]
          rebounds_df <- do.call(rbind, alt_props$selections)
        }
      }

      if (prop_type == 'rebounds ou') {
        if (length(player_rebounds$name[grepl('- Rebounds', player_rebounds$name)]) < 1) {
          next
        } else {
          ou_props <- player_rebounds[grepl('- Rebounds', player_rebounds$name), ]
          rebounds_df <- do.call(rbind, ou_props$selections)
        }
      }

      if (prop_type == 'rebounds tiers') {
        if (length(player_rebounds$name[grepl('^To Record*.*Rebounds$', player_rebounds$name)]) < 1) {
          next
        } else {
          thresh_props <- player_rebounds[grepl('^To Record*.*Rebounds$', player_rebounds$name), ]
        }
        # gotta label the rebounds values somehow, which is to use the name of the field, requiring a loop i think
        rebounds_list <- list()
        for (n  in 1:nrow(thresh_props)) {
          selections <- thresh_props$selections[[n]]
          if (nrow(selections) < 1) {
            next
          }  else {
            selections$prop_details <- thresh_props$name[[n]]
            rebounds_list[[length(rebounds_list) + 1]] <- selections
          }
        }
        rebounds_df <- do.call(rbind, rebounds_list)
      }

      # extract the description, which has the matchup, and stash in the output_list
      rebounds_df$description <- game_event$externaldescription
      output_list[[length(output_list) + 1]] <- rebounds_df
    }

    if (prop %in% c('player assists alt', 'player assists ou', 'player assists tiers',
                    'player asts alt', 'player asts ou', 'player asts tiers')) {

      # skip if no player assists props available
      prop <- gsub(' asts ', ' assists ', prop)
      market_label <- 'Player Assists'
      if (!market_label %in% unique(game_event_market_groups$name)) {
        next
      } else {
        player_assists <-
          game_event_market_groups$markets[game_event_market_groups$name == market_label][[1]]
      }

      # get correct props
      prop_type <- ifelse(grepl('alt$', prop), 'assists alt',
                   ifelse(grepl('ou$', prop), 'assists ou',
                   ifelse(grepl('tiers$', prop), 'assists tiers',
                          NA_character_
                                 )))

      # handle the different kinds of player assists bets
      if (prop_type == 'assists alt') {

        if (length(player_assists$name[grepl('Alt', player_assists$name)]) < 1) {
          next
        } else {
          alt_props <- player_assists[grepl('Alt', player_assists$name), ]
          assists_df <- do.call(rbind, alt_props$selections)
        }
      }

      if (prop_type == 'assists ou') {
        if (length(player_assists$name[grepl('- Assists', player_assists$name)]) < 1) {
          next
        } else {
          ou_props <- player_assists[grepl('- Assists', player_assists$name), ]
          assists_df <- do.call(rbind, ou_props$selections)
        }
      }

      if (prop_type == 'assists tiers') {
        if (length(player_assists$name[grepl('^To Record*.*Assists$', player_assists$name)]) < 1) {
          next
        } else {
          thresh_props <- player_assists[grepl('^To Record*.*Assists$', player_assists$name), ]
        }
        # gotta label the assists values somehow, which is to use the name of the field, requiring a loop i think
        assists_list <- list()
        for (n  in 1:nrow(thresh_props)) {
          selections <- thresh_props$selections[[n]]
          if (nrow(selections) < 1) {
            next
          }  else {
            selections$prop_details <- thresh_props$name[[n]]
            assists_list[[length(assists_list) + 1]] <- selections
          }
        }
        assists_df <- do.call(rbind, assists_list)
      }

      # extract the description, which has the matchup, and stash in the output_list
      assists_df$description <- game_event$externaldescription
      output_list[[length(output_list) + 1]] <- assists_df
    }

    if (prop %in% c('player three-pointers alt', 'player three-pointers ou', 'player three-pointers tiers',
                    'player 3pts alt', 'player 3pts ou', 'player 3pts tiers')) {

      # skip if no player threes props available
      prop <- gsub(' 3pts ', ' three-pointers ', prop)
      market_label <- 'Player Threes'
      if (!market_label %in% unique(game_event_market_groups$name)) {
        next
      } else {
        player_threes <-
          game_event_market_groups$markets[game_event_market_groups$name == market_label][[1]]
      }

      # get correct props
      prop_type <- ifelse(grepl('alt$', prop), 'three-pointers alt',
                          ifelse(grepl('ou$', prop), 'three-pointers ou',
                                 ifelse(grepl('tiers$', prop), 'three-pointers tiers',
                                        NA_character_
                                 )))

      # handle the different kinds of player threes bets
      if (prop_type == 'three-pointers alt') {

        if (length(player_threes$name[grepl('Alt', player_threes$name)]) < 1) {
          next
        } else {
          alt_props <- player_threes[grepl('Alt', player_threes$name), ]
          if (nrow(alt_props) < 1) {
            next
          } else {
            threes_df <- do.call(rbind, alt_props$selections)
          }

        }
      }

      if (prop_type == 'three-pointers ou') {
        if (length(player_threes$name[grepl('- Made Threes', player_threes$name)]) < 1) {
          next
        } else {
          ou_props <- player_threes[grepl('- Made Threes', player_threes$name), ]
          threes_df <- do.call(rbind, ou_props$selections)
        }
      }

      if (prop_type == 'three-pointers tiers') {
        if (length(player_threes$name[grepl('\\+ Made Threes', player_threes$name)]) < 1) {
          next
        } else {
          thresh_props <- player_threes[grepl('\\+ Made Threes', player_threes$name), ]
        }
        # gotta label the threes values somehow, which is to use the name of the field, requiring a loop i think
        threes_list <- list()
        for (n  in 1:nrow(thresh_props)) {
          selections <- thresh_props$selections[[n]]
          if (nrow(selections) < 1) {
            next
          }  else {
            selections$prop_details <- thresh_props$name[[n]]
            threes_list[[length(threes_list) + 1]] <- selections
          }
        }
        threes_df <- do.call(rbind, threes_list)
      }

      # extract the description, which has the matchup, and stash in the output_list
      threes_df$description <- game_event$externaldescription
      output_list[[length(output_list) + 1]] <- threes_df
    }

  }
  # if output_list is empty, error, else return as a data.frame
  if (length(output_list) == 0) {
    stop('no props returned')
  } else {
    output_df <- do.call(rbind, output_list)
  }
  return(output_df)
}

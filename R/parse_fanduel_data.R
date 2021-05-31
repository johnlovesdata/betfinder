parse_fanduel_data <- function(fanduel_data, sport, prop) {

  # loop through fanduel_data and extract the correct prop
  output_list <- list()
  for (e in names(fanduel_data)) {
    # subset the game event
    game_event <- fanduel_data[[e]]
    # extract main event
    if ('main' %in% names(game_event)) main <- game_event$main else next
    # extract attachments
    if ('attachments' %in% names(main)) main_attachments <- main$attachments else next
    # extract matchup and tipoff
    if ('events' %in% names(main_attachments)) {
      matchup <- main_attachments$events[[e]]$name
      tipoff <- main_attachments$events[[e]]$openDate
      } else {
        next
      }
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
      if (sport == 'mlb') tab_name <- 'hits_runs'
      else if (sport == 'nba') tab_name <- '1st_quarter'
      else next

      if (tab_name %in% names(game_event)) tab_content <- game_event[[tab_name]] else next
      # extract attachments
      if ('attachments' %in% names(tab_content)) tab_attachments <- tab_content$attachments else next
      # extract markets
      if ('markets' %in% names(tab_attachments)) tab_markets <- tab_attachments$markets else next
      # identify bet markets
      if (length(tab_markets) > 0) {
        bet_markets <-
          do.call(rbind, lapply(tab_markets, function(x) data.frame(id = x[['marketId']], name = x[['marketName']])))
      } else {
        next
      }
      # get the market id of the bet
      if ('Team to Score First' %in% bet_markets$name) {
        market_id <- bet_markets$id[bet_markets$name == 'Team to Score First']
        } else {
          next
        }
      # get the market of the bet
      if (market_id %in% names(tab_markets)) market <- tab_markets[[market_id]] else next
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
      ftts$tipoff <- tipoff

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
      fpts$tipoff <- tipoff
      # stash in the output_list
      output_list[[length(output_list) + 1]] <- fpts
    }

    if (prop %in% c('player points alt', 'player points ou', 'player points tiers',
                    'player pts alt', 'player pts ou', 'player pts tiers')) {
      # get the points props
      if ('player_points' %in% names(game_event)) player_points <- game_event$player_points else next
      # extract attachments
      if ('attachments' %in% names(player_points)) player_points_attachments <- player_points$attachments else next
      # extract markets
      if ('markets' %in% names(player_points_attachments)) player_points_markets <- player_points_attachments$markets else next
      # identify bet markets
      if (length(player_points_markets) > 0) {
        player_points_bet_markets <-
          do.call(rbind, lapply(player_points_markets, function(x) data.frame(id = x[['marketId']], name = x[['marketName']])))
      } else {
        next
      }
      # get the particular prop; group them by types here and fix mixed up names
      prop_type <- ifelse(grepl('alt$', prop), 'points alt',
                          ifelse(grepl('ou$', prop), 'points ou',
                                 ifelse(grepl('tiers$', prop), 'points tiers',
                                        NA_character_
                                 )))
      if (prop_type == 'points alt') {

        market_ids <- player_points_bet_markets$id[grepl('Alt Points', player_points_bet_markets$name)]
        if (length(market_ids) == 0) next
        mkt_list <- list()
        for (i in market_ids) {
          mkt <- player_points_markets[[i]]
          rnrs <- lapply(mkt$runners, function(x) {
            data.frame(prop = x[['runnerName']],
                       handicap = x[['handicap']],
                       american_odds = x[['winRunnerOdds']][['americanDisplayOdds']][['americanOdds']])
            })
          mkt_list[[length(mkt_list) + 1]] <- do.call(rbind, rnrs)
        }
      }
      if (prop_type == 'points ou') {
        market_ids <- player_points_bet_markets$id[grepl('- Points', player_points_bet_markets$name)]
        if (length(market_ids) == 0) next
        mkt_list <- list()

        for (i in market_ids) {
          mkt <- player_points_markets[[i]]
          rnrs <- lapply(mkt$runners, function(x) {
            data.frame(prop = x[['runnerName']],
                       handicap = x[['handicap']],
                       american_odds = x[['winRunnerOdds']][['americanDisplayOdds']][['americanOdds']])
          })
          mkt_list[[length(mkt_list) + 1]] <- do.call(rbind, rnrs)
        }
      }
      if (prop_type == 'points tiers') {
        market_ids <- player_points_bet_markets$id[grepl('To Score', player_points_bet_markets$name)]
        if (length(market_ids) == 0) next
        mkt_list <- list()
        for (i in market_ids) {
          mkt <- player_points_markets[[i]]
          rnrs <- lapply(mkt$runners, function(x) {
            data.frame(player = x[['runnerName']],
                       prop = mkt$marketName,
                       american_odds = x[['winRunnerOdds']][['americanDisplayOdds']][['americanOdds']])
          })
          mkt_list[[length(mkt_list) + 1]] <- do.call(rbind, rnrs)
        }
      }
      # make a data.frame
      mkt_df <- do.call(rbind, mkt_list)
      mkt_df$matchup <- matchup
      mkt_df$tipoff <- tipoff
      # stash in output_list
      output_list[[length(output_list) + 1]] <- mkt_df
    }
    if (prop %in% c('player assists alt', 'player assists ou', 'player assists tiers',
                    'player asts alt', 'player asts ou', 'player asts tiers')) {
      # get the assists props
      if ('player_assists' %in% names(game_event)) player_assists <- game_event$player_assists else next
      # extract attachments
      if ('attachments' %in% names(player_assists)) player_assists_attachments <- player_assists$attachments else next
      # extract markets
      if ('markets' %in% names(player_assists_attachments)) player_assists_markets <- player_assists_attachments$markets else next
      # identify bet markets
      if (length(player_assists_markets) > 0) {
        player_assists_bet_markets <-
          do.call(rbind, lapply(player_assists_markets, function(x) data.frame(id = x[['marketId']], name = x[['marketName']])))
      } else {
        next
      }
      # get the particular prop; group them by types here and fix mixed up names
      prop_type <- ifelse(grepl('alt$', prop), 'assists alt',
                          ifelse(grepl('ou$', prop), 'assists ou',
                                 ifelse(grepl('tiers$', prop), 'assists tiers',
                                        NA_character_
                                 )))
      if (prop_type == 'assists alt') {

        market_ids <- player_assists_bet_markets$id[grepl('Alt Assists', player_assists_bet_markets$name)]
        if (length(market_ids) == 0) next
        mkt_list <- list()
        for (i in market_ids) {
          mkt <- player_assists_markets[[i]]
          rnrs <- lapply(mkt$runners, function(x) {
            data.frame(prop = x[['runnerName']],
                       handicap = x[['handicap']],
                       american_odds = x[['winRunnerOdds']][['americanDisplayOdds']][['americanOdds']])
          })
          mkt_list[[length(mkt_list) + 1]] <- do.call(rbind, rnrs)
        }
      }
      if (prop_type == 'assists ou') {
        market_ids <- player_assists_bet_markets$id[grepl('- Assists', player_assists_bet_markets$name)]
        if (length(market_ids) == 0) next
        mkt_list <- list()

        for (i in market_ids) {
          mkt <- player_assists_markets[[i]]
          rnrs <- lapply(mkt$runners, function(x) {
            data.frame(prop = x[['runnerName']],
                       handicap = x[['handicap']],
                       american_odds = x[['winRunnerOdds']][['americanDisplayOdds']][['americanOdds']])
          })
          mkt_list[[length(mkt_list) + 1]] <- do.call(rbind, rnrs)
        }
      }
      if (prop_type == 'assists tiers') {
        market_ids <- player_assists_bet_markets$id[grepl('To Record', player_assists_bet_markets$name)]
        if (length(market_ids) == 0) next
        mkt_list <- list()
        for (i in market_ids) {
          mkt <- player_assists_markets[[i]]
          rnrs <- lapply(mkt$runners, function(x) {
            data.frame(player = x[['runnerName']],
                       prop = mkt$marketName,
                       american_odds = x[['winRunnerOdds']][['americanDisplayOdds']][['americanOdds']])
          })
          mkt_list[[length(mkt_list) + 1]] <- do.call(rbind, rnrs)
        }
      }
      # make a data.frame
      mkt_df <- do.call(rbind, mkt_list)
      mkt_df$matchup <- matchup
      mkt_df$tipoff <- tipoff

      # stash in output_list
      output_list[[length(output_list) + 1]] <- mkt_df
    }
    if (prop %in% c('player rebounds alt', 'player rebounds ou', 'player rebounds tiers',
                    'player rebs alt', 'player rebs ou', 'player rebs tiers')) {
      # get the rebounds props
      if ('player_rebounds' %in% names(game_event)) player_rebounds <- game_event$player_rebounds else next
      # extract attachments
      if ('attachments' %in% names(player_rebounds)) player_rebounds_attachments <- player_rebounds$attachments else next
      # extract markets
      if ('markets' %in% names(player_rebounds_attachments)) player_rebounds_markets <- player_rebounds_attachments$markets else next
      # identify bet markets
      if (length(player_rebounds_markets) > 0) {
        player_rebounds_bet_markets <-
          do.call(rbind, lapply(player_rebounds_markets, function(x) data.frame(id = x[['marketId']], name = x[['marketName']])))
      } else {
        next
      }
      # get the particular prop; group them by types here and fix mixed up names
      prop_type <- ifelse(grepl('alt$', prop), 'rebounds alt',
                          ifelse(grepl('ou$', prop), 'rebounds ou',
                                 ifelse(grepl('tiers$', prop), 'rebounds tiers',
                                        NA_character_
                                 )))
      if (prop_type == 'rebounds alt') {

        market_ids <- player_rebounds_bet_markets$id[grepl('Alt Rebounds', player_rebounds_bet_markets$name)]
        if (length(market_ids) == 0) next
        mkt_list <- list()
        for (i in market_ids) {
          mkt <- player_rebounds_markets[[i]]
          rnrs <- lapply(mkt$runners, function(x) {
            data.frame(prop = x[['runnerName']],
                       handicap = x[['handicap']],
                       american_odds = x[['winRunnerOdds']][['americanDisplayOdds']][['americanOdds']])
          })
          mkt_list[[length(mkt_list) + 1]] <- do.call(rbind, rnrs)
        }
      }
      if (prop_type == 'rebounds ou') {
        market_ids <- player_rebounds_bet_markets$id[grepl('- Rebounds', player_rebounds_bet_markets$name)]
        if (length(market_ids) == 0) next
        mkt_list <- list()

        for (i in market_ids) {
          mkt <- player_rebounds_markets[[i]]
          rnrs <- lapply(mkt$runners, function(x) {
            data.frame(prop = x[['runnerName']],
                       handicap = x[['handicap']],
                       american_odds = x[['winRunnerOdds']][['americanDisplayOdds']][['americanOdds']])
          })
          mkt_list[[length(mkt_list) + 1]] <- do.call(rbind, rnrs)
        }
      }
      if (prop_type == 'rebounds tiers') {
        market_ids <- player_rebounds_bet_markets$id[grepl('To Record', player_rebounds_bet_markets$name)]
        if (length(market_ids) == 0) next
        mkt_list <- list()
        for (i in market_ids) {
          mkt <- player_rebounds_markets[[i]]
          rnrs <- lapply(mkt$runners, function(x) {
            data.frame(player = x[['runnerName']],
                       prop = mkt$marketName,
                       american_odds = x[['winRunnerOdds']][['americanDisplayOdds']][['americanOdds']])
          })
          mkt_list[[length(mkt_list) + 1]] <- do.call(rbind, rnrs)
        }
      }
      # make a data.frame
      mkt_df <- do.call(rbind, mkt_list)
      mkt_df$matchup <- matchup
      mkt_df$tipoff <- tipoff

      # stash in output_list
      output_list[[length(output_list) + 1]] <- mkt_df
    }
    if (prop %in% c('player three-pointers alt', 'player three-pointers ou', 'player three-pointers tiers',
                    'player 3pts alt', 'player 3pts ou', 'player 3pts tiers')) {
      # get the three pointer props
      if ('player_threes' %in% names(game_event)) player_threes <- game_event$player_threes else next
      # extract attachments
      if ('attachments' %in% names(player_threes)) player_threes_attachments <- player_threes$attachments else next
      # extract markets
      if ('markets' %in% names(player_threes_attachments)) player_threes_markets <- player_threes_attachments$markets else next
      # identify bet markets
      if (length(player_threes_markets) > 0) {
        player_threes_bet_markets <-
          do.call(rbind, lapply(player_threes_markets, function(x) data.frame(id = x[['marketId']], name = x[['marketName']])))
      } else {
        next
      }
      # get the particular prop; group them by types here and fix mixed up names
      prop_type <- ifelse(grepl('alt$', prop), 'threes alt',
                          ifelse(grepl('ou$', prop), 'threes ou',
                                 ifelse(grepl('tiers$', prop), 'threes tiers',
                                        NA_character_
                                 )))
      if (prop_type == 'threes alt') {
        market_ids <- player_threes_bet_markets$id[grepl('Alt 3-Pointers', player_threes_bet_markets$name)]
        if (length(market_ids) == 0) next
        mkt_list <- list()
        for (i in market_ids) {
          mkt <- player_threes_markets[[i]]
          rnrs <- lapply(mkt$runners, function(x) {
            data.frame(prop = x[['runnerName']],
                       handicap = x[['handicap']],
                       american_odds = x[['winRunnerOdds']][['americanDisplayOdds']][['americanOdds']])
          })
          mkt_list[[length(mkt_list) + 1]] <- do.call(rbind, rnrs)
        }
      }
      if (prop_type == 'threes ou') {
        market_ids <- player_threes_bet_markets$id[grepl('- Made Threes', player_threes_bet_markets$name)]
        if (length(market_ids) == 0) next
        mkt_list <- list()
        for (i in market_ids) {
          mkt <- player_threes_markets[[i]]
          rnrs <- lapply(mkt$runners, function(x) {
            data.frame(prop = x[['runnerName']],
                       handicap = x[['handicap']],
                       american_odds = x[['winRunnerOdds']][['americanDisplayOdds']][['americanOdds']])
          })
          mkt_list[[length(mkt_list) + 1]] <- do.call(rbind, rnrs)
        }
      }
      if (prop_type == 'threes tiers') {
        market_ids <- player_threes_bet_markets$id[grepl('[1-9]\\+ Made Threes', player_threes_bet_markets$name)]
        if (length(market_ids) == 0) next
        mkt_list <- list()
        for (i in market_ids) {
          mkt <- player_threes_markets[[i]]
          rnrs <- lapply(mkt$runners, function(x) {
            data.frame(player = x[['runnerName']],
                       prop = mkt$marketName,
                       american_odds = x[['winRunnerOdds']][['americanDisplayOdds']][['americanOdds']])
          })
          mkt_list[[length(mkt_list) + 1]] <- do.call(rbind, rnrs)
        }
      }
      # make a data.frame
      mkt_df <- do.call(rbind, mkt_list)
      mkt_df$matchup <- matchup
      mkt_df$tipoff <- tipoff

      # stash in output_list
      output_list[[length(output_list) + 1]] <- mkt_df
    }
  }
  # if output_list is empty, error, else return as a data.frame
  if (length(output_list) == 0) {
    stop('no fanduel ', prop, ' props returned')
  } else {
    output_df <- do.call(rbind, output_list)
  }

  return(output_df)
}

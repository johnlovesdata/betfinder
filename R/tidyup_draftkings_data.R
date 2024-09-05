tidyup_draftkings_data <- function(draftkings_data, sport, prop = FALSE, game_lines = FALSE,
                                   key = get_key_path(sport = sport, prop = prop, game_lines = game_lines)) {
  # make the output using the input
  output_df <- draftkings_data
  output_df <- output_df[!is.na(output_df$providerOutcomeId), ]

  if (game_lines == TRUE) {

    # fix the totals first
    output_df$newlabel <- output_df$label
    totals <- na.omit(output_df[output_df$bet_type == 'Total', ])
    new_totals_list <- list()
    for (m in unique(totals$matchup)) {
      mu <- totals[totals$matchup == m, ]
      teams <- unlist(strsplit(m, ' @ '))
      mu$newlabel[[1]] <- teams[[1]]
      mu$newlabel[[2]] <- teams[[2]]
      new_totals_list[[length(new_totals_list) + 1]] <- mu
    }
    new_totals <- dplyr::bind_rows(new_totals_list)
    output_df <- dplyr::bind_rows(new_totals, output_df[output_df$bet_type != 'Total', ])
    output_df <- output_df[!is.na(output_df$providerOutcomeId), ]

    output_df$tidyteam <- ifelse(output_df$bet_type == 'Total', output_df$matchup, output_df$newlabel)

    output_df$tidyplayer <- 'team'
    output_df$tidytype <- output_df$bet_type
    output_df$tidyamericanodds <- as.numeric(gsub('//+', '', output_df$oddsAmerican))
    output_df$tidyline <- as.numeric(output_df$line)
    output_df$tidyou <- ifelse(output_df$label %in% c('Over', 'over'), 'over',
                               ifelse(output_df$label %in% c('Under', 'under'), 'under', NA_character_))

  }

  # for each prop, append whatever tidy fields we can, which should make thte data useful across datasets
  if (prop %in% c('first team to score', 'ftts')) {
    # generate tidy names and odds
    output_df$tidyteam <- output_df$label
    output_df$tidyplayer <- 'team'
    output_df$tidyamericanodds <- as.numeric(output_df$oddsAmerican)
    # for flexible prop names, specify the value explicitly here
    output_df$prop <- 'first team to score'
  }
  if (prop %in% c('game go to ot', 'game go to overtime')) {
    # generate tidy names and odds
    output_df$tidyteam <- 'game'
    output_df$tidyplayer <- 'game'
    output_df$tidyou <- output_df$label
    output_df$tidyamericanodds <- as.numeric(output_df$oddsAmerican)
    # for flexible prop names, specify the value explicitly here
    output_df$prop <- 'game go to ot'
  }

  if (prop %in% c('first player to score', 'fpts')) {
    # set tidyplayer and tidyamericanodds
    if ('participant' %in% names(output_df)) {
      hacky_tidyplayer <- as.character(output_df$participant)
    } else if ('label' %in% names(output_df)) {
      hacky_tidyplayer <- as.character(output_df$label)
    }
    output_df$tidyplayer <- hacky_tidyplayer
    output_df$tidyamericanodds <- as.numeric(output_df$oddsAmerican)
    # for flexible props, specify the value explicitly here
    output_df$prop <- 'first player to score'
  }
  if (prop %in% c('player any td', 'player first td')) {
    # generate tidy names and odds
    output_df$tidyplayer <- unlist(output_df$label)
    output_df$tidyamericanodds <- as.numeric(output_df$oddsAmerican)
  }
  if (prop %in% c('player any goal','goals', 'player first goal', 'player last goal')) {
    criterion <- switch(prop,
                        `goals` = 'Anytime Scorer',
                        `player any goal` = 'Anytime Scorer',
                        `player first goal` = 'First Scorer',
                        `player last goal` = 'Last Scorer')
    output_df <- output_df[output_df$criterionName == criterion, ]
    output_df$tidyplayer <- output_df$label
    output_df$tidyamericanodds <- as.numeric(output_df$oddsAmerican)
  }


  if (grepl(' ou$| $tiers|points|rebounds|assists|three-pointers| pts| 3pts| rebs| asts| blocks| steals| turnovers|runs|strikeout| hr|hit|rbi|double|pass|rush|att|shots|saves|fg|kicking|pat', tolower(prop))) {
    # get names
    if (sport != 'nhl') {
      hacky_tidyplayer <- (as.character(output_df$participant))
      output_df$tidyplayer <- hacky_tidyplayer
    } else {
      output_df$tidyplayer <- output_df$participant
    }
    # get tidy ou from label
    output_df$tidyou <- ifelse(grepl('Over', as.character(output_df$label)), 'over',
                               ifelse(grepl('Under', as.character(output_df$label)), 'under',
                                      ifelse(grepl('^no$', tolower(as.character(output_df$label))), 'no',
                                             ifelse(grepl('^yes$', tolower(as.character(output_df$label))), 'yes',
                                                    output_df$label
                                             ))))
    # get tidy line from the label
    num_part <- as.numeric(gsub('[A-Za-z| ]', '', output_df$label))
    output_df$tidyline <- num_part
    if (all(is.na(output_df$tidyline)) & ('line' %in% names(output_df))) {
      output_df$tidyline <- as.numeric(output_df$line)
    }
    # get the tidy odds
    output_df$tidyamericanodds <- as.numeric(output_df$oddsAmerican)

  }

  # tidyup the matchup! use the team abbreviations from the lookup
  matchup_list <- strsplit(gsub(' vs ', ' @ ', output_df$matchup), ' @ ')
  if (sport != 'nhl') {
    output_df$tidyawayteam <- unlist(lapply(matchup_list, '[[', 1))
    output_df$tidyhometeam <- unlist(lapply(matchup_list, '[[', 2))
  }

  # tidyup the date! make sure this is EST
  output_df$tidygamedatetime <- lubridate::as_datetime(output_df$tipoff) - lubridate::hours(4)
  output_df$tidygamedatetime <- lubridate::round_date(output_df$tidygamedatetime, "30 minutes")
  lubridate::tz(output_df$tidygamedatetime) <- 'EST'

  # filter out the cols we don't need, i.e. not tidy ones
  names_to_keep <- names(output_df)[grepl('tidy|prop', names(output_df))]
  output_df <- output_df[, names(output_df) %in% names_to_keep]

  # stamp it up
  output_df$site <- 'draftkings'
  output_df$sport <- sport
  if (!'prop' %in% names(output_df)) {
    output_df$prop <- prop
  }

  output_df <- output_df[!duplicated(output_df), ]


  # deliver
  return(output_df)

}


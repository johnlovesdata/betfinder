#' get data for dashboard
#' @importFrom magrittr %>%
#' @param loc chr
#' @return list
#' @export
get_data_for_dashboard <- function(loc = c('local', 'server')) {

  # get config with paths
  path_list <- jsonlite::fromJSON(system.file('config', 'dashboard', loc, 'config.json', package = 'betfinder'))

  # projections ----
  projections <-
    # first player to score
    read.csv(path_list$fpts_proj_path) %>%
    # first team to score
    dplyr::bind_rows(
      read.csv(path_list$ftts_proj_path)) %>%
    ## HERE IS WHERE WE CAN CURATE THE FIELDS INCLUDED FOR ADDITIONAL CONTEXT
    dplyr::select(sport, tidyplayer, tidyteam, prop, projected_prob, jumper_injury_status) %>%
    dplyr::group_by(tidyteam) %>%
    tidyr::fill(jumper_injury_status, .direction = 'updown') %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      sport = sport,
      jumper = dplyr::if_else(grepl('team', prop), tidyplayer, NA_character_),
      tidyplayer = dplyr::if_else(grepl('team', prop), 'team', tidyplayer),
      tidyteam = tidyteam,
      prop = prop,
      projected_prob = projected_prob,
      jumper_injury_status)

  # player data ----
  player_data <-
    # rosters
    read.csv(path_list$rosters_path) %>%
    dplyr::mutate(
      tidyplayer = normalize_names(PLAYER_NAME, key = system.file('lu', 'nba', 'player', 'lu.json', package = 'betfinder')),
      tidyteam = normalize_names(TEAM_ABBREVIATION, key = system.file('lu', 'nba', 'team', 'lu.json', package = 'betfinder'))) %>%
    dplyr::select(-PLAYER_NAME, -TEAM_ABBREVIATION) %>%
    # lineups
    dplyr::left_join(
      read.csv(path_list$lineups_path) %>%
        dplyr::mutate(tidyteam = normalize_names(TEAM_ABBREVIATION, key = system.file('lu', 'nba', 'team', 'lu.json', package = 'betfinder')),
               tidyplayer = normalize_names(PLAYER_NAME, key = system.file('lu', 'nba', 'player', 'lu.json', package = 'betfinder'))) %>%
        dplyr::select(-PLAYER_NAME, -TEAM_ABBREVIATION),
      by = c('tidyteam', 'tidyplayer')) %>%
    dplyr::group_by(tidyplayer) %>%
    tidyr::fill(tidyteam, .direction = 'updown') %>%
    dplyr::ungroup() %>%
    dplyr::group_by(tidyplayer) %>%
    dplyr::arrange(desc(LINEUP_DESC)) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      sport = 'nba',
      tidyplayer,
      tidyteam,
      injury_status = TO_PLAY_DESC,
      starter_status = LINEUP_DESC
      )

  output_list <- list(
    projections = projections,
    player_data = player_data
  )

  return(output_list)

}

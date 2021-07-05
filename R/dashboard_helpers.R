#' Utility functions for the dashboard
#' @name dashboard_helpers
NULL

get_rosters <- function(dashboard_config = NULL) {
  # error out if missing an arg
  if (is.null(dashboard_config)) stop('get_rosters() needs dashboard_config arg')
  config_names <- names(dashboard_config)
  rosters <- list()
  if ('nba_rosters_path' %in% config_names) {
    nba_rosters <- read.csv(dashboard_config$nba_rosters_path)
    nba_tidy <- data.frame(
      sport = 'nba',
      tidyteam = normalize_names(nba_rosters$TEAM_ABBREVIATION, key = get_key_path('nba', 'team')),
      tidyplayer = normalize_names(nba_rosters$PLAYER_NAME, key = get_key_path('nba', 'player'))
    )
    rosters[['nba']] <- nba_tidy
  }
  if ('mlb_rosters_path' %in% config_names) {
    mlb_rosters <- read.csv(dashboard_config$mlb_rosters_path)
    mlb_tidy <- data.frame(
      sport = 'mlb',
      tidyteam = normalize_names(mlb_rosters$abbrev, key = get_key_path('mlb', 'team')),
      tidyplayer = normalize_names(mlb_rosters$name, key = get_key_path('mlb', 'player'))
    )
    rosters[['mlb']] <- mlb_tidy

  }
  rosters <- do.call(rbind, rosters)
  return(rosters)
}

get_projections <- function(dashboard_config = NULL) {

  # error out if missing an arg
  if (is.null(dashboard_config)) stop('get_projections() needs dashboard_config arg')
  config_names <- names(dashboard_config)
  projections <- list()
  if ('nba_ftts_proj_path' %in% config_names) {
    nba_ftts_proj <- read.csv(dashboard_config$nba_ftts_proj_path)
    nba_ftts_tidy <- data.frame(
      sport = 'nba',
      prop = 'ftts',
      tidyplayer = 'team',
      tidyteam = nba_ftts_proj$tidyteam,
      tidyline = nba_ftts_proj$projected_line,
      projected_prob = nba_ftts_proj$projected_prob)
    projections[[length(projections) + 1]] <- nba_ftts_tidy
  }
  if ('nba_fpts_proj_path' %in% config_names) {
    nba_fpts_proj <- read.csv(dashboard_config$nba_fpts_proj_path)
    nba_fpts_tidy <- data.frame(
      sport = 'nba',
      prop = 'fpts',
      tidyplayer = nba_fpts_proj$tidyplayer,
      tidyteam = nba_fpts_proj$tidyteam,
      tidyline = nba_fpts_proj$projected_line,
      projected_prob = nba_fpts_proj$projected_prob)
    projections[[length(projections) + 1]] <- nba_fpts_tidy
  }
  # warn if returning nothing
  if (length(projections) == 0) warning('returning an empty data.frame!')
  projections <- do.call(rbind, projections)
  return(projections)
}

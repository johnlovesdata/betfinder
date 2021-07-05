#' get data for dashboard
#' @importFrom magrittr %>%
#' @param loc chr where this is being run
#' @return list
#' @export
get_data_for_dashboard <- function(loc = c('local', 'server')) {

  # get config with paths ----
  if (!(loc %in% c('local', 'server'))) {
    stop('unrecognized loc argument')
  }
  dashboard_config <- jsonlite::fromJSON(
    system.file('config', 'dashboard', loc, 'config.json', package = 'betfinder'))
  config_names <- names(dashboard_config)
  # player data ----
  if (!any(grepl('roster', config_names))) {
    stop('no rosters specified in dashboard config; make sure you are pointing to the right config file')
  }
  rosters <- get_rosters(dashboard_config = dashboard_config)
  # projections ----
  if (any(grepl('proj', config_names))) {
    projections <- get_projections(dashboard_config = dashboard_config)
  } else {
    warning('no projections specified in dashboard config - make sure this is expected')
    return(list(rosters = rosters))
  }

  # output ----
  output_list <- list(
    projections = projections,
    rosters = rosters
  )

  return(output_list)

}

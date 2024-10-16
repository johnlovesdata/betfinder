#' get data from dk api
#'
#' @param sport chr
#' @param save_path chr
#' @param sleep_time num
#' @return list
#' @export
get_draftkings_data <- function(sport, save_path = NULL, sleep_time = 0) {

  # set the eventgroup based on the sport
  if (sport == 'mlb') eg <- 84240
  if (sport == 'nba') eg <- 42648
  if (sport == 'nfl') eg <- 88808
  if (sport == 'nhl') eg <- 42133
  if (sport == 'wnba') eg <- 94682

  # main request
  ## uri
  main_uri <- paste0('https://sportsbook-nash-usil.draftkings.com/sites/US-IL-SB/api/v5/eventgroups/', eg)
  ## headers
  main_hdrs <- c(
    'Accept'='*/*',
    'Accept-Encoding'='gzip, deflate, br, zstd',
    'Accept-Language'='en-US,en;q=0.9',
    'Origin'='https://sportsbook.draftkings.com',
    'Priority'='u=1, i',
    'Referer'='https://sportsbook.draftkings.com/',
    'Sec-Ch-Ua'='"Not)A;Brand";v="99", "Google Chrome";v="127", "Chromium";v="127"',
    'Sec-Ch-Ua-Mobile'='?0',
    'Sec-Ch-Ua-Platform'='"macOS"',
    'Sec-Fetch-Dest'='empty',
    'Sec-Fetch-Mode'='cors',
    'Sec-Fetch-Site'='same-site', 
    'User-Agent'='Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36'
  )
  ## query
  main_query <- list(
    'format'='json')
  ## stitch together request
  main_req <- httr2::request(main_uri) |>
    httr2::req_headers(!!!main_hdrs) |>
    httr2::req_url_query(!!!main_query)
  ## perform the request and get the json
  main_resp <- httr2::req_perform(main_req)
  main_content <- main_resp |>
    httr2::resp_body_json()

  # extract event ids
  event_group <- main_content[['eventGroup']]
  events <- event_group[['events']]
  event_ids <- unique(sapply(events, function(x) x[['eventId']]))

  # loop through all the event ids and grab the event json
  event_list <- list()
  for (e in event_ids) {
    ## sleep if needed
    Sys.sleep(sleep_time)
    ## event uri
    ### recycles headers and query
    event_uri <- paste0('https://sportsbook-nash-usil.draftkings.com/sites/US-IL-SB/api/v3/event/', e)
    event_req <- httr2::request(event_uri) |>
      httr2::req_headers(!!!main_hdrs) |>
      httr2::req_url_query(!!!main_query)
    ## perform the request and get the json
    event_resp <- httr2::req_perform(event_req)
    event_content <- event_resp |>
      httr2::resp_body_json()
    event_list[[as.character(e)]] <- event_content
  }
  if (!is.null(save_path)) {
    fn <- paste0(sport, '_draftkings_', e, '_', as.numeric(Sys.time()), '.json')
    jsonlite::write_json(event_list, file.path(save_path, fn))
    R.utils::gzip(file.path(save_path, fn), ext='gz')
  }
  # return
  return(event_list)
}

#' Utility functions to make things work better
#' @param x Vector of strings to normalize in \code{normalize_names}
#' @param key List of elements containing fields "name" and "aliases" in \code{normalize_names}
#' @param warn Optional, Boolean -- warn if a name in X was not found in the Key names in n\code{normalize_names}
#' @note \code{normalize_names} was sniped from jimtheflash/gambling_stuff and written by anthonyshook (to be clear, it was written by a hook owned by some fella named anthony)
#' @name utils
NULL

#' @rdname utils
normalize_names <- function(x, key, warn = TRUE) {

  # Allow for passing the json file directly, otherwise KEY is assumed to be a list
  if (is.character(key) &&
      grepl(pattern = '\\.json$', x = 'string.json')) {
    key <- jsonlite::read_json(key)
  }

  # Fun warning times
  # Does every element contain two fields - name and aliases?
  field_check <- sapply(key, function(k) {
    cnames <- names(k)
    # Check on names
    if (!identical(cnames, c('name','aliases'))) {
      stop("Each element of 'key' must contain exactly two fields, 'name' and 'aliases'")
    }
    # If names exist, check types
    if (!is.character(k$name)) stop("Each element 'name' must be a string")
    if (!is.list(k$aliases)) stop("Each element 'aliases' must be a list of strings")


  })

  # Run through and do the work
  for (i in seq_along(key)) {
    # For every element, GSUB
    patterns <- paste0('^', key[[i]]$aliases, '$', collapse="|")
    x <- gsub(pattern = patterns,
              replacement = key[[i]]$name,
              x = x)
  }

  # Yell if there is something in the list that was not in the names OR aliases
  # We can just look at Names now, because if it WAS in the aliases, it would
  # have been replaced by a name.  So any leftover item in the list that isn't
  # in "all_names" is something we didn't expect to see
  all_names <- sapply(key, '[[', 'name')
  if (!all(x %in% all_names) && warn == TRUE) {
    msg <- paste0("X contained Strings not found in 'name' - ",
                  paste(x[!(x %in% all_names)], collapse = ', '))
    warning(msg)
  }

  # Return the string
  return(x)
}

#' @rdname utils
get_key_path <- function(sport, prop) {

  # set the list of props for different key_types
  # TODO: set this via a config probably (another json)
  key_type <- character()
  if (prop %in% c('first team to score', 'ftts')) {
    key_type <- 'team'
  } else if (prop %in% c('first player to score', 'fpts')) {
    key_type <- 'player'
  }

  system.file('lu', sport, key_type, 'lu.json', package = 'betfinder')

}

#' @rdname utils
hacky_tidyup_player_names <- function(player_names) {

  output <- iconv(player_names, to = "ASCII//TRANSLIT")
  output <- tolower(gsub('[^[:alnum:]]', '', output))
  output <- gsub('jr$|sr$|ii$|iii$', '', output)


  return(output)

}

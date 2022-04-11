#' Utility functions to make things work better
#' @param x Vector of strings to normalize in \code{normalize_names}
#' @param key List of elements containing fields "name" and "aliases" in \code{normalize_names}
#' @param warn Optional, Boolean -- warn if a name in X was not found in the Key names in \code{normalize_names}
#' @param uri main url for call to \code{httr::GET}
#' @param query list of paramaters to pass to \code{httr::GET}
#' @note \code{normalize_names} was sniped from \code{jimtheflash/gambling_stuff} and written by anthonyshook (to be clear, it was written by a hook owned by some fella named anthony)
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
get_key_path <- function(sport, prop, game_lines = FALSE) {

  # set the list of props for different key_types
  # TODO: set this via a config probably (another json)
  key_type <- character()
  if (game_lines == TRUE) {
    key_type <- 'team'
  } else if (grepl('player|fpts', prop)) {
      key_type <- 'player'
  } else if (grepl('team|ftts', prop)) {
    key_type <- 'team'
  }

  system.file('lu', sport, key_type, 'lu.json', package = 'betfinder')

}

#' @rdname utils
hacky_tidyup_player_names <- function(player_names) {
  output <- iconv(player_names, to = "ASCII//TRANSLIT")
  output <- gsub(' \\(.*.', '', output)
  output <- tolower(gsub('[^[:alnum:]]', '', output))
  output <- gsub('jr$|sr$|ii$|iii$', '', output)
  return(output)
}

#' @rdname utils
get_content <- function(uri, query) {
  resp <- httr::GET(uri, query = query, encode = 'json')
  output <- httr::content(resp)
  return(output)
}

#' @rdname utils
#' @export
american_to_prob <- function(odds) {
  output <- list()
  for (o in odds) {
    if (is.na(o)) { e <- NA_real_ }
    else if (o < 0) { e <- (o * -1) / ((o * -1) + 100) }
    else if (o > 0) { e <- 100 / (o + 100) }
    else e <- { NA_real_ }
    output[[length(output) + 1]] <- e
  }
  return(unlist(output))
}

#' @rdname utils
#' @export
prob_to_american <- function(probs) {
  output <- list()
  for (p in probs) {
    if (is.na(p)) { e <- NA_real_ }
    else if (p <= .5) { e <- (100 / p) - 100 }
    else if (p > .5) { e <- (p / (1 - p)) * -100 }
    else {e <- NA_real_}
    output[[length(output) + 1]] <- e
  }
  return(unlist(output))
}

#' @rdname utils
#' @export
list_df_to_df <- function(df) {

  df_names <- names(df)
  new_df <- as.data.frame(matrix(nrow = nrow(df), ncol = ncol(df)))
  names(new_df) <- df_names
  for (i in df_names) {
    if (class(df[[i]]) == 'list') {
      new_df[[i]] <- unlist(df[[i]])
    } else {
      new_df[[i]] <- df[[i]]
    }
  }
  return(new_df)
}


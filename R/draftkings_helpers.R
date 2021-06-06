parse_dk_prop <- function(props_obj, prop_name, prop, prop_obj_names) {

    if (!prop_name %in% prop_obj_names) stop('no draftkings ', prop, ' available')
    subsetted_props <- props_obj[[which(prop_obj_names == prop_name)]]$offerSubcategory$offers

    output_list <- list()
    for (event_props in subsetted_props) {

      ids <- as.data.frame(event_props[[1]][grepl('Id', names(event_props[[1]]))])
      ids$providerOfferId <- NULL

      for (ep in event_props) {
        outcomes <- as.data.frame(apply(do.call(rbind, ep$outcomes), 2, unlist))
        if (is.null(outcomes$participant)) next
        outcomes <- merge(outcomes, ids)
        output_list[[length(output_list) + 1]] <- outcomes

      }
    }

    return(output_list)
}

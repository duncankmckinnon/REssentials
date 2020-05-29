
#' generate summary for a given attribute grouped by a target attribute
#'
#' @param data a data frame or tibble type object
#' @param summary_attr a column label as a string to summarize
#' @param group_attr a column label to group by before summarizing (optional)
#' @returns a grouped summary table for the attribute
attribute.summary <- function(data, summary_attr, group_attr = NULL){
  if(is.null(group_attr)) {
    response <- suppressWarnings(dplyr::select_(data, summary_attr))
  } else {
    response <- suppressWarnings(dplyr::group_by_(dplyr::select_(data, summary_attr, group_attr), group_attr) )
  }
  return( attribute.stats(response, summary_attr) )
}

#' get the summary statistics for an attribute (grouped or otherwise)
#'
#' @param data a data frame or tibble type object
#' @param stats_attr a column label as a string to get stats for
#' @returns a tibble with the summary stats for the specified column/grouping
attribute.stats <- function(data, stats_attr){
  attribute_types <- attribute.types(data)
  stats_attr_ <- as.name(stats_attr)
  if( attribute_types[stats_attr] %in% c('integer', 'numeric', 'complex', 'double') ){
    return( suppressWarnings(
      dplyr::summarise_( data,
                         min = lazyeval::interp(~min(var, na.rm = T), var = stats_attr_),
                         p25 = lazyeval::interp(~quantile(var, probs=0.25, na.rm = T), var = stats_attr_),
                         mean = lazyeval::interp(~mean(var, na.rm = T), var = stats_attr_),
                         median = lazyeval::interp(~median(var, na.rm = T), var = stats_attr_),
                         p75 = lazyeval::interp(~quantile(var, probs=0.75, na.rm = T), var = stats_attr_),
                         max = lazyeval::interp(~max(var, na.rm = T), var = stats_attr_),
                         sd = lazyeval::interp(~sd(var, na.rm = T), var = stats_attr_))))
  } else if( attribute_types[stats_attr] %in% c('factor', 'logical', 'character') ){
    return( suppressWarnings(dplyr::count_( data, stats_attr_)))
  }
}

#' get the type of attribute in each column of the data, as a vector
#'
#' @param data a data frame or tibble type object
#' @returns a character vector of the class type of each column
attribute.types <- function(data){
  sapply(data, class, USE.NAMES = T)
}

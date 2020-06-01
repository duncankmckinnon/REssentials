

#' generate summary for a given attribute grouped by a target attribute
#'
#' @param data a data frame or tibble type object
#' @param summary_attr a column label as a string to summarize
#' @param group_attr a column label to group by before summarizing (optional)
#' @returns a grouped summary table for the attribute(s)
#' @examples
#' # summarise all columns
#' attribute.summary(iris)
#'
#' # summarise only 'Petal.Length'
#' attribute.summary(iris, 'Petal.Length')
#'
#' # summarise 'Petal.Length' grouped by 'Species'
#' attribute.summary(iris, 'Petal.Length', 'Species')
#'
#' # summarise all columns grouped by 'Species'
#' attribute.summary(iris, group_attr = 'Species')
attribute.summary <- function(data, summary_attr = NULL, group_attr = NULL, .checkAssertions = T){

  # check assertions ( used in first attributes request )
  if( .checkAssertions ) .assertions(data, c(summary_attr, group_attr))

  # collect column names needed for summary
  if( !missing(summary_attr) ) {
    if( !missing(group_attr) ){
      attr_names <- c(summary_attr, group_attr)
    } else {
      attr_names <- summary_attr
    }
  } else {
    attr_names <- names(data)
  }

  # select columns to use in summarise
  response_data <- suppressWarnings(dplyr::select_at(data, .vars = vars(attr_names)))
  return( attributes.summarise( response_data, group_attr, F ) )
}

#' generate a summary of each attribute in the dataset grouped by a target attribute
#'
#' @param data a data frame or tibble type object
#' @param group_attr a column label as a string to group by before summarizing (optional)
#' @examples
#' # summarise all columns
#' attributes.summarise(iris)
#'
#' # summarise all columns grouped by 'Species'
#' attributes.summarise(iris, 'Species')
attributes.summarise <- function(data, group_attr = NULL, .checkAssertions = T){

  # check assertions ( used in first attributes request )
  if( .checkAssertions ) .assertions(data, group_attr)

  # get columns to summarize
  attributes <- names(data)

  # group data by column
  if( !is.null(group_attr) ){
    group_attr_ <- as.symbol(group_attr)
    data <- data %>% group_by_(group_attr_)

    # remove grouping column from summary columns
    attributes <- attributes[ attributes != group_attr ]
  }
  return( sapply(attributes, function(x) attribute.stats(data, x, F), USE.NAMES = T, simplify = F) )
}

#' get the summary statistics for an attribute (grouped or otherwise)
#'
#' @param data a data frame or tibble type object
#' @param stats_attr a column label as a string to get stats for
#' @returns a tibble with the summary stats for the specified column/grouping
#' @examples
#' sample.data <- data.frame('chr' = sample(c('a','b','c'), 100, T),
#'                           'int' = sample(c(1,2,3), 100, T),
#'                           'dttm' = sample(as.Date.character(paste('2020-01-', 1:30, sep = '')), 100, T))
#'
#' # get category type stats
#' attribute.stats(sample.data, 'chr')
#'
#' # get numeric type stats
#' attribute.stats(sample.data, 'int')
#'
#' # get date type stats
#' attribute.stats(sample.data, 'dttm')
attribute.stats <- function(data, stats_attr = NULL, .checkAssertions = T){

  # for requests where column is already selected - syntactic ease of use
  if( missing(stats_attr) && length(data) == 1 ) stats_attr <- names(data)

  # check assertions ( used in first attributes request )
  if( .checkAssertions ) .assertions(data, stats_attr, T)

  # get class for each column in summary data
  attribute_types <- attribute.class(data, F)
  stats_attr_ <- as.symbol(stats_attr)

  # summarise column by class
  if( attribute_types[stats_attr] %in% c('integer', 'numeric', 'complex', 'double') ){

    # summarise numeric type attributes
    return( suppressWarnings(
      ### need to use deprecated dplyr tools until updated
      dplyr::summarise_( data,
                         min = lazyeval::interp(~min(var, na.rm = T), var = stats_attr_),
                         p25 = lazyeval::interp(~quantile(var, probs=0.25, names = F, na.rm = T), var = stats_attr_),
                         mean = lazyeval::interp(~mean(var, na.rm = T), var = stats_attr_),
                         median = lazyeval::interp(~median(var, na.rm = T), var = stats_attr_),
                         p75 = lazyeval::interp(~quantile(var, probs=0.75, names = F, na.rm = T), var = stats_attr_),
                         max = lazyeval::interp(~max(var, na.rm = T), var = stats_attr_),
                         sd = lazyeval::interp(~sd(var, na.rm = T), var = stats_attr_))) %>% as_tibble())
  } else if( attribute_types[stats_attr] == 'Date' ){

    # summarise datetime type attributes
    return( suppressWarnings(
      ### need to use deprecated dplyr tools until updated
      dplyr::summarise_( data,
                         min = lazyeval::interp(~min(var, na.rm = T), var = stats_attr_),
                         p25 = lazyeval::interp(~quantile(var, probs=0.25, names = F, type = 1, na.rm = T), var = stats_attr_),
                         mean = lazyeval::interp(~mean(var, na.rm = T), var = stats_attr_),
                         median = lazyeval::interp(~median(var, na.rm = T), var = stats_attr_),
                         p75 = lazyeval::interp(~quantile(var, probs=0.75, names = F, type = 1, na.rm = T), var = stats_attr_),
                         max = lazyeval::interp(~max(var, na.rm = T), var = stats_attr_),
                         sd = lazyeval::interp(~sd(var, na.rm = T), var = stats_attr_)) %>% as_tibble()))
  } else {

    # summarise categorical type attributes
    return( suppressWarnings(
      ### need to use deprecated dplyr tools until updated
      dplyr::tally_( data, stats_attr_, name = 'count')))
  }
}

#' get the class for each column in data, as a named character vector
#'
#' @param data a data frame or tibble type object
#' @returns a character vector of the class type of each column
#' @examples
#' sample.data <- data.frame('chr' = sample(c('a','b','c'), 100, T),
#'                           'int' = sample(c(1,2,3), 100, T),
#'                           'dttm' = sample(as.Date.character(paste('2020-01-', 1:30, sep = '')), 100, T))
#'
#' # get data classes for all columns
#' attribute.class(sample.data)
attribute.class <- function(data, .checkAssertions = T){
  # check assertions ( used in first attributes request )
  if( .checkAssertions ) .assertions(data)

  # get classes for all attributes
  return( sapply(data, class, USE.NAMES = T, simplify = T) )
}

.assertions <- function(data, attributes = NULL, require.attributes = F){
  assertthat::assert_that( !missing(data) )
  assertthat::assert_that( is.data.frame(data) )
  if( require.attributes ) assertthat::assert_that(all( !is.null( attributes ) ))
  if( !missing(attributes) ) assertthat::assert_that(all( is.character( attributes ) ) && all( attributes %in% names(data) ))
}

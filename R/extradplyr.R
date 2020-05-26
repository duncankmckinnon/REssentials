
# all purpose function to generate summary statistics for a given attribute grouped by a target attribute
attribute_stats <- function(data, summary_attr, group_attr = NULL){
  assertthat::assert_that(is.data.frame(data))
  assertthat::assert_that(is.string(summary_attr))
  summary_attr_ <- as.name(summary_attr)
  if(is.null(group_attr)) {
    response <- data %>%
      dplyr::select_(summary_attr_)
  } else {
    assertthat::assert_that(is.string(group_attr))
    group_attr_ <- as.name(group_attr)
    response <- data %>%
      dplyr::select_(summary_attr_, group_attr_) %>%
      dplyr::group_by_(group_attr_)
  }
  return(
    response %>%
        dplyr::summarise_(
          min = interp(~min(var, na.rm = T), var = summary_attr_),
          p25 = interp(~quantile(var, probs=0.25, na.rm = T), var = summary_attr_),
          mean = interp(~mean(var, na.rm = T), var = summary_attr_),
          median = interp(~median(var, na.rm = T), var = summary_attr_),
          p75 = interp(~quantile(var, probs=0.75, na.rm = T), var = summary_attr_),
          max = interp(~max(var, na.rm = T), var = summary_attr_),
          sd = interp(~sd(var, na.rm = T), var = summary_attr_)
        )
  )
}


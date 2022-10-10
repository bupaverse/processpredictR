#' Assign column outcome with outcome labels based on last activity from the prefixed dataframe
#'
#'(WIP)
#'
#'@param prefix_df A dataframe with prefixes and last activity column from create_prefix_df function
#'@param ... A named character vector to assign outcome labels to the end activities
#'
#' @export
assign_outcome_labels <- function(prefix_df, ...) {
  prefix_df %>%
    mutate(outcome = forcats::fct_collapse(last_activity, ...))
}

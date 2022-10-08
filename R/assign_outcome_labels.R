#' Assign column outcome with outcome labels based on last activity from the prefixed dataframe
#'
#'(WIP)
#'
#' @export
assign_outcome_labels <- function(prefix_df, ...) {
  prefix_df %>%
    mutate(outcome = forcats::fct_collapse(last_activity, ...))
}

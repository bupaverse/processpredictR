#' @title Assign outcome labels
#'
#' @description A shortcut for creating outcome labels based on its column values.
#'
#'@param processed_df [`ppred_examples_df`]: A processed dataset returned by `prepare_examples()`.
#'@param ... A named character vector to assign outcome labels to the end activities of outcome
#'
#' @export
assign_outcome_labels <- function(processed_df, ...) {
  outcome <- NULL

  processed_df %>%
    mutate(outcome = forcats::fct_collapse(outcome, ...))
}

#' Prepare event log for the Transformer model fit
#'
#'(WIP)
#'
#' @param log An object of class log
#' @param ... A named character vector to assign outcome labels to the end activities
#'
#' @examples
#' acts <- eventdataR::patients %>% edeaR::end_activities("activity") # look on distributions of end activities per case
#' acts <- unique(acts$activity) %>% as.character()
#' preprocess_log(eventdataR::patients, outcome_label1 = "Check-out", outcome_label2 = acts)
#'
#' @export
preprocess_log <- function(log, ...) {
  UseMethod("preprocess_log")
}

#' @export
preprocess_log.log <- function(log, ...) {

  processed_df <- create_prefix_df(log)
  processed_df <- processed_df %>% assign_outcome_labels(...) #assigns labels by end activity
  processed_df

}

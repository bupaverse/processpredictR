#' Prepare event log for the Transformer model fit
#'
#'
#'
#' @param log An object of class log
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

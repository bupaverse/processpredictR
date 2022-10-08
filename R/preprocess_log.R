
preprocess_log <- function(log, ...) {
  UseMethod("preprocess_log")
}

preprocess_log.log <- function(log, ...) {

  processed_df <- create_prefix_df(log)
  processed_df <- processed_df %>% assign_outcome_labels(...) #assigns labels by end activity
  processed_df

}

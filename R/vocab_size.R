#' Calculate the vocabulary size, i.e. the sum of number of activities, outcome labels and padding keys
#'
#'(WIP)
#'
#'@param processed_df A processed dataframe from create_prefix_df() or preprocess_log()
#'
#'@return an Integer number of vocabulary size to define the transformer model
#'
#'@export
vocab_size <- function(processed_df) {
  # maybe calculate vocab_size using preprocess_log()

  activity_names <- processed_df$current_activity %>% unique() %>% as.character()
  activity_names <- c("PAD", "UNK") %>%
    append(activity_names)
  outcome_names <- processed_df$next_activity %>% unique()
  length(unique(append(activity_names, outcome_names))) +1 %>% as.integer()

}

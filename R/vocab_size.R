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

  if ("outcome" %in% names(processed_df)) {
    activity_names <- processed_df$current_activity %>% unique() %>% as.character()
    activity_names <- c("PAD", "UNK") %>%
      append(activity_names)
    outcome_names <- processed_df$outcome %>% unique() %>% as.character()
    length(unique(append(activity_names, outcome_names))) %>% as.integer()

  }

  else if ("next_activity" %in% names(processed_df)) {
    activity_names <- processed_df$current_activity %>% unique() %>% as.character()
    activity_names <- c("PAD", "UNK") %>%
      append(activity_names)
    outcome_names <- processed_df$next_activity %>% unique()
    length(unique(append(activity_names, outcome_names))) %>% as.integer()

  }

  else if ("next_time" %in% names(processed_df) || "remaining_time" %in% names(processed_df)) {
    activity_names <- processed_df[[bupaR::activity_id(processed_df)]] %>% as.character() %>% unique()
    activity_names <- c("PAD", "UNK") %>%
      append(activity_names)
    length(unique(activity_names)) %>% as.integer()

  }

  else if ("remaining_trace" %in% names(processed_df)) {
    activity_names <- processed_df[[bupaR::activity_id(processed_df)]] %>% as.character() %>% unique()
    activity_names <- c("PAD", "UNK") %>%
      append(activity_names)
    length(unique(activity_names)) %>% as.integer()


  }

}

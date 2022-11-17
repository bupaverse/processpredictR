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
  task <- get_task(processed_df)

  if (task == "outcome") {
    activity_names <- processed_df[[bupaR::activity_id_(processed_df)]] %>% unique() %>% as.character()
    activity_names <- c("PAD", "UNK") %>%
      append(activity_names)
    outcome_names <- processed_df$outcome %>% unique() %>% as.character()
    length(unique(append(activity_names, outcome_names))) %>% as.integer()
  }

  else if (task == "next_activity") {
    activity_names <- processed_df[[bupaR::activity_id_(processed_df)]] %>% unique() %>% as.character()
    activity_names <- c("PAD", "UNK") %>%
      append(activity_names)
    outcome_names <- processed_df$next_activity %>% unique()
    length(unique(append(activity_names, outcome_names))) %>% as.integer()
  }

  else if (task %in% c("next_time", "remaining_time", "remaining_trace")) {
    activity_names <- processed_df[[bupaR::activity_id(processed_df)]] %>% as.character() %>% unique()
    activity_names <- c("PAD", "UNK") %>%
      append(activity_names)
    length(unique(activity_names)) %>% as.integer()
  }

  # else if (task == "remaining_trace") {
  #   activity_names <- processed_df[[bupaR::activity_id(processed_df)]] %>% as.character() %>% unique()
  #   activity_names <- c("PAD", "UNK") %>%
  #     append(activity_names)
  #   length(unique(activity_names)) %>% as.integer()

}

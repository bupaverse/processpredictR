#' Create a vocabulary, i.e. c("PAD", "UNK"), activities, outcome labels
#'
#'@param processed_df A processed dataframe
#'@param type A type of vocabulary c("both", "activities", "outcomes")
#'@return a character vector of unique elements
#'
#' @export
create_vocabulary <- function(processed_df, task_type) {

  if (task_type == "outcome") {
    activity_names <- processed_df$current_activity %>% unique() %>% as.character()
    activity_names <- c("PAD", "UNK") %>%
      append(activity_names)
    outcome_names <- processed_df$outcome %>% unique() %>% as.character()
    values_x <- unique(append(activity_names, outcome_names))

    #outcome tokens
    keys_y <- outcome_names %>% as.list()
    outs <- data.frame(activity = unlist(keys_y)) %>% mutate(key_id = row_number() - 1)

    #activities tokens
    keys_x <- as.list(values_x)
    acts <- data.frame(activity_name = keys_x %>% unlist()) %>% mutate(key_id = row_number() - 1)

    list(activities = acts, outcomes = outs)

  }

  else if (task_type == "next_activity") {
    activity_names <- processed_df$current_activity %>% unique() %>% as.character()
    activity_names <- c("PAD", "UNK") %>%
      append(activity_names)
    outcome_names <- processed_df$next_activity %>% unique()
    values_x <- unique(append(activity_names, outcome_names))

    #activities tokens
    keys_x <- as.list(values_x)
    acts <- data.frame(activity_name = keys_x %>% unlist()) %>% mutate(key_id = row_number() - 1)

    #outcome tokens
    keys_y <- outcome_names %>% as.list()
    outs <- data.frame(activity = unlist(keys_y)) %>% mutate(key_id = row_number() - 1)

    list(activities = acts, outcomes = outs)

  }

  else if (task_type == "next_time" || task_type == "remaining_time") {
    activity_names <- processed_df[[bupaR::activity_id(processed_df)]] %>% as.character() %>% unique()
    activity_names <- c("PAD", "UNK") %>%
      append(activity_names)
    values_x <- unique(activity_names)

    #activities tokens
    keys_x <- as.list(values_x)
    acts <- data.frame(activity_name = keys_x %>% unlist()) %>% mutate(key_id = row_number() - 1)

    acts

  }

}





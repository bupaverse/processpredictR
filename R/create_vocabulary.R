#' Create a vocabulary, i.e. c("PAD", "UNK"), activities, outcome labels
#'
#' @param processed_df A processed dataframe
#' @return a character vector of unique elements
#'
#' @examples
#' tasks <- c("outcome", "next_activity", "next_time", "remaining_time", "remaining_trace")
#' purrr::map(tasks, ~create_prefix_df(patients, prediction = .x)) %>% purrr::map(create_vocabulary)
#'
#' @export
create_vocabulary <- function(processed_df) {

  #OUTCOME
  if ("outcome" %in% names(processed_df)) {
    activity_names <- processed_df$current_activity %>% unique() %>% as.character()
    activity_names <- c("PAD", "UNK") %>%
      append(activity_names)
    outcome_names <- processed_df$outcome %>% unique() %>% as.character()
    values_x <- unique(append(activity_names, outcome_names))

    #outcome tokens
    keys_y <- outcome_names %>% as.list()
    #outs <- data.frame(activity = unlist(keys_y)) %>% mutate(key_id = row_number() - 1)

    #activities tokens
    keys_x <- as.list(values_x)
    #acts <- data.frame(activity_name = keys_x %>% unlist()) %>% mutate(key_id = row_number() - 1)

    list(keys_x = keys_x, keys_y = keys_y)

  }

  #NEXT_ACTIVITY
  else if ("last_activity" %in% names(processed_df)) {
    activity_names <- processed_df$current_activity %>% unique() %>% as.character()
    activity_names <- c("PAD", "UNK") %>%
      append(activity_names)
    outcome_names <- processed_df$next_activity %>% unique()
    values_x <- unique(append(activity_names, outcome_names))

    #activities tokens
    keys_x <- as.list(values_x)
    #acts <- data.frame(activity_name = keys_x %>% unlist()) %>% mutate(key_id = row_number() - 1)

    #outcome tokens
    keys_y <- outcome_names %>% as.list()
    #outs <- data.frame(activity = unlist(keys_y)) %>% mutate(key_id = row_number() - 1)

    list(keys_x = keys_x, keys_y = keys_y)

  }

  #NEXT_TIME & REMAINING_TIME
  else if ("next_time" %in% names(processed_df) || "remaining_time" %in% names(processed_df)) {
    activity_names <- processed_df[[bupaR::activity_id(processed_df)]] %>% as.character() %>% unique()
    activity_names <- c("PAD", "UNK") %>%
      append(activity_names)
    values_x <- unique(activity_names)

    #activities tokens
    keys_x <- as.list(values_x)
    #acts <- data.frame(activity_name = keys_x %>% unlist()) %>% mutate(key_id = row_number() - 1)

    #acts
    keys_x

  }

  #REMAINING TRACE
  else if ("remaining_trace" %in% names(processed_df)) {

    activity_names <- processed_df[[bupaR::activity_id(processed_df)]] %>% as.character() %>% unique()
    activity_names <- c("PAD", "UNK") %>%
      append(activity_names)
    values_x <- unique(activity_names)

    #activities tokens
    keys_x <- as.list(values_x)
    #acts <- data.frame(activity_name = keys_x %>% unlist()) %>% mutate(key_id = row_number() - 1)

    keys_y <- processed_df$remaining_trace %>% unique() %>% as.list()
    #outs <- data.frame(activity = unlist(keys_y)) %>% mutate(key_id = row_number() - 1)

    list(keys_x = keys_x, keys_y = keys_y)
  }

}





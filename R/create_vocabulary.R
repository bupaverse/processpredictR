#' Create a vocabulary, i.e. c("PAD", "UNK"), activities, outcome labels
#'
#'@param processed_df A processed dataframe
#'@param type A type of vocabulary c("both", "activities", "outcomes")
#'@return a character vector of unique elements
#'
#' @export
create_vocabulary <- function(processed_df, type = "both") {

  if ("outcome" %in% names(processed_df)) {
    activity_names <- processed_df$current_activity %>% unique() %>% as.character()
    activity_names <- c("PAD", "UNK") %>%
      append(activity_names)
    outcome_names <- processed_df$outcome %>% unique() %>% as.character()
    values_x <- unique(append(activity_names, outcome_names))


  }


  else if ("next_activity" %in% names(processed_df)) {
    activity_names <- processed_df$current_activity %>% unique() %>% as.character()
    activity_names <- c("PAD", "UNK") %>%
      append(activity_names)
    outcome_names <- processed_df$next_activity %>% unique()
    values_x <- unique(append(activity_names, outcome_names))

  }

  #activities tokens
  keys_x <- as.list(values_x)

  #outcome tokens
  keys_y <- outcome_names %>% as.list()


  acts <- data.frame(activity_name = keys_x %>% unlist()) %>% mutate(key_id = row_number() - 1)
  outs <- data.frame(activity = unlist(keys_y)) %>% mutate(key_id = row_number() - 1)



  if (type == "both") {
    list(activities = acts, outcomes = outs)
  }

  else if (type == "activities") {
    acts
  }

  else if (type == "outcomes") {
    outs
  }

}





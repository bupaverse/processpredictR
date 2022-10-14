#' Create a vocabulary, i.e. c("PAD", "UNK"), activities, outcome labels
#'
#'@param processed_df A processed dataframe
#'@return a character vector of unique elements
#'
#' @export
create_vocabulary <- function(processed_df) {

  if ("outcome" %in% names(processed_df)) {
    activity_names <- processed_df$current_activity %>% unique() %>% as.character()
    activity_names <- c("PAD", "UNK") %>%
      append(activity_names)
    outcome_names <- processed_df$outcome %>% unique() %>% as.character()
    unique(append(activity_names, outcome_names))

  }


  else if ("next_activity" %in% names(processed_df)) {
    activity_names <- processed_df$current_activity %>% unique() %>% as.character()
    activity_names <- c("PAD", "UNK") %>%
      append(activity_names)
    outcome_names <- processed_df$next_activity %>% unique()
    unique(append(activity_names, outcome_names))

  }
}





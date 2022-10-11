#' Calculate the vocabulary size, i.e. the sum of number of activities, outcome labels and padding keys
#'
#'(WIP)
#'
#'@param log An object of a class log
#'@param outcome_names A character vector of outcome labels
#'
#'@return an Integer number of vocabulary size to define the transformer model
#'
#'@export
vocab_size <- function(log, outcome_names) {
  UseMethod("vocab_size")
}

#'@export
vocab_size.log <- function(log, outcome_names) {
  # maybe calculate vocab_size using preprocess_log()

  activity_names <- log %>% activity_labels() %>% as.character()
  activity_names <- c("PAD", "UNK") %>%
    append(activity_names)
  length(unique(append(activity_names, outcome_names)))  %>% as.integer()

}

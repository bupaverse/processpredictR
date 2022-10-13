#' Create transformer model from log
#'
#' An alternative to transformer_model() using only log (WIP)
#'
#' @param log An object of class log
#'
#' @return a defined transformer model
#'
transformer_model_log <- function(log) {
  UseMethod("transformer_model_log")
}

transformer_model_log.log <- function(log, vocabulary_size, number_of_outputs) {
  #max_case_length()
  maxlen <- log %>%
    trace_length() %>%
    max() %>% as.integer()

  #vocab_size
  activity_names <- log %>% activity_labels() %>% as.character()
  activity_names <- c("PAD", "UNK") %>%
    append(activity_names)
  length(unique(append(activity_names, outcome_names)))  %>% as.integer()

  #num_outputs




}

vocab_size <- function(log, outcome_names) {
  UseMethod("vocab_size")
}

vocab_size.log <- function(log, outcome_names) {
  # maybe calculate vocab_size using preprocess_log()

  activity_names <- log %>% activity_labels() %>% as.character()
  activity_names <- c("PAD", "UNK") %>%
    append(activities)
  length(unique(append(activity_names, outcome_names)))  %>% as.integer()

}

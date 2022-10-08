max_case_length <- function(log) {
  UseMethod("max_case_length")
}

max_case_length.log <- function(log) {
  log %>%
    trace_length() %>%
    max() %>% as.integer()

  #processed_df$traces %>% lengths() %>% max()    #for processed_df as input
}

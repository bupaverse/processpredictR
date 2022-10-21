#' Calculate the maximum length of a case / number of activities in the longest trace in an event log.
#'
#'(WIP)
#'
#'@param processed_df A preprocessed dataframe from create_prefix_df().
#'@return An integer number of the maximum case length (longest trace) in an event log.
#'
#'@export
max_case_length <- function(processed_df) {

  if ("outcome" %in% names(processed_df) || "next_activity" %in% names(processed_df)) {
    processed_df$traces %>% lengths() %>% max()
  }

  else if ("next_time" %in% names(processed_df) || "remaining_time" %in% names(processed_df)) {
    processed_df %>% trace_length() %>% max() %>% as.integer()
  }

}


#max_case_length <- function(log) {
#   UseMethod("max_case_length")
# }
#

# max_case_length.log <- function(log) {
#   log %>%
#     trace_length() %>%
#     max() %>% as.integer()
#
#   #processed_df$traces %>% lengths() %>% max()    #for processed_df as input
# }

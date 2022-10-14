#' Create a dataframe with prefixes
#'
#' @param log An object of class log.
#' @param prediction A prediction task. A character string from c("outcome", "next_activity", "...").
#' @param ... Assign outcome labels to each case based on end activities. If not specified, simply defines
#' outcome as last activity in a trace.
#' @return a dataframe.
#' @examples
#' library(eventdataR)
#' create_prefix_df(patients, prediction = "outcome", outcome1 = "Check-out",
#' outcome2 = c("X-Ray", "Blood test", "Triage and Assessment"))
#'
#' acts <- patients %>% edeaR::end_activities("activity")
#' acts <- unique(acts$activity) %>% as.character()
#' create_prefix_df(eventdataR::patients, prediction = "outcome", outcome_label1 = "Check-out",
#' outcome_label2 = acts[-1])
#'
#' create_prefix_df(traffic_fines, prediction = "next_activity")
#'
#' @export
create_prefix_df <- function(log, prediction, ...) {
  UseMethod("create_prefix_df")
}

#' @export
create_prefix_df.log <- function(log, prediction, ...) {
  # traces_per_case <- case_list(log, .keep_trace_list = TRUE) %>%
  #   mutate(ith_case = row_number()) %>%
  #   rename(case_id = case_id(log))


  if (prediction == "outcome") {

    traces_per_case <- case_list(log, .keep_trace_list = TRUE) %>%
      mutate(ith_case = row_number()) %>%
      rename(case_id = case_id(log))

    case_prefix <- traces_per_case %>%
      group_by(ith_case, case_id) %>%
      #group_by(across(c(5, 1))) %>%
      summarise(prefix = purrr::accumulate(traces_per_case$trace_list[[ith_case]], paste, sep = " - "),
                current_activity = traces_per_case$trace_list[[ith_case]],
                traces = traces_per_case$trace_list[ith_case]) %>%
      mutate(k = row_number() - 1,
             outcome = last(current_activity),
             next_activity = lead(current_activity)) %>%
      filter(!is.na(next_activity)) %>%
      ungroup()

    case_prefix <- case_prefix %>% assign_outcome_labels(...)
    case_prefix


  }
  else if (prediction == "next_activity") {
    traces_per_case <- case_list(log, .keep_trace_list = TRUE) %>%
      mutate(trace_list = purrr::map(trace_list, ~append(.x, "endpoint")),
             ith_case = row_number()) %>%
      rename(case_id = case_id(log))

    case_prefix <- traces_per_case %>%
      group_by(ith_case, case_id) %>%
      #group_by(across(c(5, 1))) %>%
      summarise(prefix = purrr::accumulate(traces_per_case$trace_list[[ith_case]], paste, sep = " - "),
                current_activity = traces_per_case$trace_list[[ith_case]],
                traces = traces_per_case$trace_list[ith_case]) %>%
      mutate(k = row_number() - 1,
             last_activity = last(current_activity),
             next_activity = lead(current_activity)) %>% #,
      filter(!is.na(next_activity)) %>%
             #next_activity = if_else(is.na(next_activity), "endpoint", next_activity)) %>%
      ungroup()

    case_prefix

  }

}

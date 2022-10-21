#' Create a dataframe with prefixes
#'
#' @param log An object of class log.
#' @param prediction A prediction task. A character string from c("outcome", "next_activity", "next_time", "...").
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
#' create_prefix_df(patients, prediction = "next_time")
#' create_prefix_df(patients, prediction = "remaining_time")
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

  else if (prediction == "next_time") {

    to_activitylog(log) %>% # situation when there is both start- and end timestamps
      group_by(!!bupaR:::case_id_(log)) %>%
      mutate(ith_case = cur_group_id(),
             activity_duration = (complete-start) %>% as.numeric(),
             time_passed = cumsum(activity_duration),
             trace = paste(handling, collapse = ","),
             k = row_number() - 1) %>% # getting the traces
      arrange(!!bupaR:::case_id_(log)) %>%
      mutate(#!!bupaR:::case_id_(log),
             prefix = purrr::accumulate(as.character(!!bupaR:::activity_id_(log)), paste, sep = " - "),
             #!!bupaR:::activity_id_(log),
             activity_duration = activity_duration
             ) %>%
      mutate(latest_time = activity_duration,
             next_time = lead(activity_duration),
             recent_time = lag(activity_duration),
             recent_time = if_else(is.na(recent_time), 0, recent_time)) %>%
      drop_na(next_time) %>%
      select(ith_case, !!bupaR:::case_id_(log), prefix, k, time_passed, recent_time, latest_time, next_time, activity_duration, trace, everything()) %>%
      re_map(to_activitylog(log) %>% mapping())

  }



  else if (prediction == "remaining_time") {

    to_activitylog(log) %>% # situation when there is both start- and end timestamps
      group_by(!!bupaR:::case_id_(log)) %>%
      mutate(ith_case = cur_group_id(),
             activity_duration = (complete-start) %>% as.numeric(),
             time_passed = cumsum(activity_duration),
             trace = paste(handling, collapse = ","),
             k = row_number() - 1) %>% # getting the traces
      arrange(!!bupaR:::case_id_(log)) %>%
      mutate(#!!bupaR:::case_id_(log),
        prefix = purrr::accumulate(as.character(!!bupaR:::activity_id_(log)), paste, sep = " - "),
        #!!bupaR:::activity_id_(log),
        activity_duration = activity_duration
      ) %>%
      mutate(latest_time = activity_duration,
             recent_time = lag(activity_duration),
             recent_time = if_else(is.na(recent_time), 0, recent_time),
             remaining_time = last(time_passed) - time_passed[k+1]) %>%
      select(ith_case, !!bupaR:::case_id_(log), prefix, k, time_passed, recent_time, latest_time, remaining_time, activity_duration, trace, everything()) %>%
      re_map(to_activitylog(log) %>% mapping())

  }

}

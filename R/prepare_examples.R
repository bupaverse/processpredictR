#' Preprocess an event log.
#'
#' @description Prepare an event log for a transformer prediction task.
#'
#' @param log \code{\link{log}}: Object of class \code{\link{log}} or derivatives (\code{\link{grouped_log}}, \code{\link{eventlog}},
#' \code{\link{activitylog}}, etc.).
#' @param task [`character`] (default `"outcome"`): A prediction task (`"outcome"`, `"next_activity"`,
#' `"next_time"`,`"remaining_time"`, `"remaining_trace"`).
#' @param ... Assign outcome labels to each case based on end activity. If not specified, simply defines
#' outcome as last activity in a trace.
#'
#' @examples
#' # Outcome task prediction with custom outcome labels:
#' library(eventdataR)
#' acts <- patients %>% edeaR::end_activities("activity")
#' acts <- unique(acts$activity) %>% as.character()
#' prepare_examples(eventdataR::patients, task = "outcome", outcome_label1 = "Check-out",
#' outcome_label2 = acts[-1])
#'
#' # Example next_activity task prediction on traffic_fines dataset:
#' prepare_examples(traffic_fines, prediction = "next_activity")
#'
#' # Each prediction task for the patients dataset:
#' tasks <- c("outcome", "next_activity", "next_time", "remaining_time", "remaining_trace")
#' purrr::map(tasks, ~prepare_examples(eventdataR::patients, task = .x))
#'
#' @export
prepare_examples <- function(log, task = c("outcome", "next_activity",
                                           "next_time", "remaining_time",
                                           "remaining_trace"), ...) {
  UseMethod("prepare_examples")
}

#' @export
prepare_examples.log <- function(log, task = c("outcome", "next_activity",
                                               "next_time", "remaining_time",
                                               "remaining_trace"), ...) {
  # traces_per_case <- case_list(log, .keep_trace_list = TRUE) %>%
  #   mutate(ith_case = row_number()) %>%
  #   rename(case_id = case_id(log))

  #task <- match.arg(task)

  task <- rlang::arg_match(task)

  cat("Prediction task: ", task, "\n")

  if (task == "outcome") {

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
  else if (task == "next_activity") {
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

  else if (task == "next_time") {

    if ((log[[lifecycle_id(log)]] %>% unique() %>% length()) == 2) {

      to_activitylog(log) %>% # situation when there is both start- and end timestamps
        group_by(!!bupaR:::case_id_(log)) %>%
        mutate(ith_case = cur_group_id(),
               activity_duration = (complete-start) %>% as.numeric(),
               time_passed = cumsum(activity_duration),
               trace = paste(!!bupaR:::activity_id_(log), collapse = ","),
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

  }



  else if (task == "remaining_time") { # MAKE HERE AN IF ELSE STATEMENT TO CHECK FOR START- END TIMESTAMPS

    if ((log[[lifecycle_id(log)]] %>% unique() %>% length()) == 2) {

      to_activitylog(log) %>% # situation when there is both start- and end timestamps
        group_by(!!bupaR:::case_id_(log)) %>%
        mutate(ith_case = cur_group_id(),
               activity_duration = (complete-start) %>% as.numeric(),
               time_passed = cumsum(activity_duration),
               trace = paste(!!bupaR:::activity_id_(log), collapse = ","),
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

    else {print("Log does not contain both start- and end timestamps")}

  }

  else if (task == "remaining_trace") {

    log %>% # situation when there is both start- and end timestamps
      group_by(!!bupaR:::case_id_(log)) %>%
      mutate(ith_case = cur_group_id(),
             trace = paste(!!bupaR:::activity_id_(log), collapse = ","),
             k = row_number() - 1) %>% # getting the traces
      arrange(!!bupaR:::case_id_(log)) %>%
      distinct(!!bupaR:::activity_instance_id_(log), .keep_all = T) %>%
      mutate(
        prefix = purrr::accumulate(as.character(!!bupaR:::activity_id_(log)), paste, sep = " - ", .dir = "forward"),
        remaining_trace = purrr::accumulate(as.character(!!bupaR:::activity_id_(log)), paste, sep = " - ", .dir = "backward")) %>%
      mutate(remaining_trace = lead(remaining_trace),
             remaining_trace = if_else(is.na(remaining_trace), "endpoint", remaining_trace)) %>%
      select(ith_case, !!bupaR:::case_id_(log), prefix, remaining_trace, k, trace, everything()) %>%
      re_map(mapping(log))

  }

}

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
#' prepare_examples(traffic_fines, task = "next_activity")
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

  message("Prediction task: ", task, "\n")

  # OUTCOME
  if (task == "outcome") {

    traces_per_case <- case_list(log, .keep_trace_list = TRUE) %>%
      mutate(ith_case = row_number()) %>%
      rename(case_id = case_id(log))

    traces_per_case %>%
      group_by(ith_case, case_id) %>%
      summarise(prefix_list = purrr::accumulate(traces_per_case$trace_list[[ith_case]], c),
                prefix = purrr::accumulate(traces_per_case$trace_list[[ith_case]], paste, sep = " - "),
                current_activity = traces_per_case$trace_list[[ith_case]],
                traces = traces_per_case$trace_list[ith_case]) %>%
      mutate(k = row_number() - 1,
             outcome = last(current_activity)) %>%
      slice(-n()) %>%
      ungroup() %>%
      assign_outcome_labels(...) -> output

    class(output) <- c("ppred_examples_df", class(output))

    attr(output, "task") <- task
    attr(output, "y_var") <- "outcome"
    attr(output, "features") <- NULL
    attr(output, "vocabulary") <- create_vocabulary(output)

    return(output)
    #RETURNS OBJECT OF TYPE `log`
    # log %>%
    #   group_by(!!bupaR:::case_id_(log)) %>%
    #   mutate(ith_case = cur_group_id(),
    #          trace = paste(!!bupaR:::activity_id_(log), collapse = ","),
    #          k = row_number() - 1) %>% # getting the traces
    #   arrange(!!bupaR:::case_id_(log)) %>%
    #   distinct(!!bupaR:::activity_instance_id_(log), .keep_all = T) %>%
    #   mutate(
    #     prefix = purrr::accumulate(as.character(!!bupaR:::activity_id_(log)), paste, sep = " - ", .dir = "forward")) %>%
    #   mutate(next_activity = lead(!!bupaR:::activity_id_(log)),
    #          next_activity = if_else(is.na(next_activity), "endpoint", as.character(next_activity)),
    #          outcome = last(!!bupaR:::activity_id_(log)),
    #          current_activity = !!bupaR:::activity_id_(log)) %>%
    #   select(ith_case, !!bupaR:::case_id_(log), prefix, next_activity, k, trace, everything()) %>%
    #   assign_outcome_labels(...) -> output


  }

  # NEXT_ACTIVITY
  else if (task == "next_activity") {

    traces_per_case <- case_list(log, .keep_trace_list = TRUE) %>%
      mutate(trace_list = purrr::map(trace_list, ~append(.x, "endpoint")),
             ith_case = row_number()) %>%
      rename(case_id = case_id(log))

    traces_per_case %>%
      group_by(ith_case, case_id) %>%
      #group_by(across(c(5, 1))) %>%
      summarise(prefix_list = purrr::accumulate(traces_per_case$trace_list[[ith_case]], c),
                prefix = purrr::accumulate(traces_per_case$trace_list[[ith_case]], paste, sep = " - "),
                current_activity = traces_per_case$trace_list[[ith_case]],
                traces = traces_per_case$trace_list[ith_case]) %>%
      mutate(k = row_number() - 1,
             next_activity = lead(current_activity)) %>% #,
      filter(!is.na(next_activity)) %>%
             #next_activity = if_else(is.na(next_activity), "endpoint", next_activity)) %>%
      ungroup() -> output

    #RETURNS OBJECT OF TYPE `log`
    # log %>%
    #   group_by(!!bupaR:::case_id_(log)) %>%
    #   mutate(ith_case = cur_group_id(),
    #          trace = paste(!!bupaR:::activity_id_(log), collapse = ","),
    #          k = row_number() - 1) %>% # getting the traces
    #   arrange(!!bupaR:::case_id_(log)) %>%
    #   distinct(!!bupaR:::activity_instance_id_(log), .keep_all = T) %>%
    #   mutate(
    #     prefix = purrr::accumulate(as.character(!!bupaR:::activity_id_(log)), paste, sep = " - ", .dir = "forward")) %>%
    #   mutate(next_activity = lead(!!bupaR:::activity_id_(log)),
    #          next_activity = if_else(is.na(next_activity), "endpoint", as.character(next_activity)),
    #          last_activity = last(next_activity)) %>%
    #   select(ith_case, !!bupaR:::case_id_(log), prefix, next_activity, k, trace, everything()) %>%
    #   re_map(mapping(log)) -> output
    class(output) <- c("ppred_examples_df", class(output))

    attr(output, "task") <- task
    attr(output, "y_var") <- "next_activity"
    attr(output, "features") <- NULL
    attr(output, "mapping") <- mapping(log)
    attr(output, "vocabulary") <- create_vocabulary(output)
  }

  # NEXT_TIME
  else if (task == "next_time") {

    if(is.eventlog(log)) {
      log <- to_activitylog(log)
    }


    # if contains both start- and end timestamps
    # if ((log[[lifecycle_id(log)]] %>% unique() %>% length()) == 2) {
#
#       to_activitylog(log) %>%
#         group_by(!!bupaR:::case_id_(log)) %>%
#         mutate(ith_case = cur_group_id(),
#                activity_duration = (complete-start) %>% as.numeric(),
#                time_passed = cumsum(activity_duration),
#                trace = paste(!!bupaR:::activity_id_(log), collapse = ","),
#                k = row_number() - 1) %>% # getting the traces
#         arrange(!!bupaR:::case_id_(log)) %>%
#         mutate(#!!bupaR:::case_id_(log),
#           prefix = purrr::accumulate(as.character(!!bupaR:::activity_id_(log)), paste, sep = " - "),
#           #!!bupaR:::activity_id_(log),
#           activity_duration = activity_duration
#         ) %>%
#         mutate(latest_time = activity_duration,
#                next_time = lead(activity_duration),
#                recent_time = lag(activity_duration),
#                recent_time = if_else(is.na(recent_time), 0, recent_time)) %>%
#         drop_na(next_time) %>%
#         select(ith_case, !!bupaR:::case_id_(log), prefix, k, time_passed, recent_time, latest_time, next_time, activity_duration, trace, everything()) %>%
#         re_map(to_activitylog(log) %>% mapping()) -> output

      traces_per_case <- case_list(log, .keep_trace_list = TRUE)

      log %>%
        left_join(traces_per_case, by = case_id(log)) %>%
        # if start is missing, set equal to complete
        # if complete is missing, set equal to start
        mutate(start = if_else(is.na(start), complete, start),
               complete = if_else(is.na(complete), start, complete)) %>%
        group_by_case() %>%
        arrange(!!bupaR:::case_id_(log)) %>%
        mutate(ith_case = cur_group_id(),
               activity_duration = as.double(complete - start),
               k = row_number() - 1,
               prefix_list = purrr::accumulate(as.character(!!bupaR:::activity_id_(log)), c),
               prefix = purrr::accumulate(as.character(!!bupaR:::activity_id_(log)), paste, sep = " - "),
               latest_duration = activity_duration,
               throughput_time = as.double(complete - min(start)),
               processing_time = cumsum(activity_duration),
               time_before_activity = as.double(start - lag(complete)),
               time_till_next_activity = as.double(lead(start) - complete)) %>%
        ungroup() %>%
        mutate(time_before_activity = if_else(is.na(time_before_activity), 0, time_before_activity),
               latest_duration = ifelse(is.na(latest_duration), 0, latest_duration)) %>%
        drop_na(time_till_next_activity)  %>%
        select(ith_case, !!bupaR:::case_id_(log), prefix_list, prefix, k,
               latest_duration, throughput_time, time_before_activity,
               processing_time, time_till_next_activity, trace) -> output
#    # if does NOT contain both start- and end timestamps
    # else if ((log[[lifecycle_id(log)]] %>% unique() %>% length()) == 1) {
    #
    #   log %>%
    #     group_by(!!bupaR:::case_id_(log)) %>%
    #     mutate(ith_case = cur_group_id(),
    #            start = lag(!!bupaR:::timestamp_(log)),
    #            complete = (!!bupaR:::timestamp_(log)),
    #            start = if_else(is.na(start), complete, start),
    #            activity_duration = (complete-start) %>% as.numeric(),
    #            time_passed = cumsum(activity_duration),
    #            trace = paste(!!bupaR:::activity_id_(log), collapse = ","),
    #            k = row_number() - 1) %>% # getting the traces
    #     arrange(!!bupaR:::case_id_(log)) %>%
    #     mutate(#!!bupaR:::case_id_(log),
    #       prefix = purrr::accumulate(as.character(!!bupaR:::activity_id_(log)), paste, sep = " - "),
    #       #!!bupaR:::activity_id_(log),
    #       activity_duration = activity_duration
    #     ) %>%
    #     mutate(latest_time = activity_duration,
    #            next_time = lead(activity_duration),
    #            recent_time = lag(activity_duration),
    #            recent_time = if_else(is.na(recent_time), 0, recent_time)) %>%
    #     drop_na(next_time) %>%
    #     select(ith_case, !!bupaR:::case_id_(log), prefix, k, time_passed, recent_time, latest_time, next_time, activity_duration, trace, everything()) %>%
    #     re_map(mapping(log)) -> output

      class(output) <- c("ppred_examples_df", class(output))

      attr(output, "task") <- task
      attr(output, "y_var") <- "time_till_next_activity"
      attr(output, "features") <- c("latest_duration", "throughput_time","time_before_activity","processing_time")
      attr(output, "mapping") <- mapping(log)
      attr(output, "vocabulary") <- create_vocabulary(output)
  }

  # REMAINING_TIME
  else if (task == "remaining_time") {


    if(is.eventlog(log)) {
      log <- to_activitylog(log)
    }

    traces_per_case <- case_list(log, .keep_trace_list = TRUE)

    log %>%
      left_join(traces_per_case, by = case_id(log))  %>%
      mutate(start = if_else(is.na(start), complete, start),
             complete = if_else(is.na(complete), start, complete)) %>%
      group_by(!!bupaR:::case_id_(log)) %>%
      arrange(!!bupaR:::case_id_(log)) %>%
      mutate(ith_case = cur_group_id(),
             activity_duration = (complete-start) %>% as.numeric(),
             prefix_list = purrr::accumulate(as.character(!!bupaR:::activity_id_(log)), c),
             prefix = purrr::accumulate(as.character(!!bupaR:::activity_id_(log)), paste, sep = " - "),
             k = row_number() - 1,
             latest_duration = activity_duration,
             processing_time = cumsum(activity_duration),
             throughput_time = as.double(complete - min(start)),
             #next_time = lead(activity_duration),
             previous_duration = lag(activity_duration),
             previous_duration = if_else(is.na(previous_duration), 0, previous_duration),
             remaining_time = last(complete) -complete) %>%
      select(ith_case, !!bupaR:::case_id_(log), prefix_list, prefix, k,
             throughput_time, processing_time,
             previous_duration, remaining_time, trace, everything()) -> output

    # if contains both start- and end timestamps
    # if ((log[[lifecycle_id(log)]] %>% unique() %>% length()) == 2) {
    #
    #   to_activitylog(log) %>%
    #     group_by(!!bupaR:::case_id_(log)) %>%
    #     mutate(ith_case = cur_group_id(),
    #            activity_duration = (complete-start) %>% as.numeric(),
    #            time_passed = cumsum(activity_duration),
    #            trace = paste(!!bupaR:::activity_id_(log), collapse = ","),
    #            k = row_number() - 1) %>% # getting the traces
    #     arrange(!!bupaR:::case_id_(log)) %>%
    #     mutate(#!!bupaR:::case_id_(log),
    #       prefix = purrr::accumulate(as.character(!!bupaR:::activity_id_(log)), paste, sep = " - "),
    #       #!!bupaR:::activity_id_(log),
    #       activity_duration = activity_duration
    #     ) %>%
    #     mutate(latest_time = activity_duration,
    #            recent_time = lag(activity_duration),
    #            recent_time = if_else(is.na(recent_time), 0, recent_time),
    #            remaining_time = last(time_passed) - time_passed[k+1]) %>%
    #     select(ith_case, !!bupaR:::case_id_(log), prefix, k, time_passed, recent_time, latest_time, remaining_time, activity_duration, trace, everything()) %>%
    #     re_map(to_activitylog(log) %>% mapping()) -> output
    #
    # }
    #
    # # if does NOT contain both start- and end timestamps
    # else if ((log[[lifecycle_id(log)]] %>% unique() %>% length()) == 1) {
    #
      # log %>%
      #   group_by(!!bupaR:::case_id_(log)) %>%
      #   mutate(ith_case = cur_group_id(),
      #          start = lag(!!bupaR:::timestamp_(log)),
      #          complete = (!!bupaR:::timestamp_(log)),
      #          start = if_else(is.na(start), complete, start),
      #          activity_duration = (complete-start) %>% as.numeric(),
      #          time_passed = cumsum(activity_duration),
      #          trace = paste(!!bupaR:::activity_id_(log), collapse = ","),
      #          k = row_number() - 1) %>% # getting the traces
      #   arrange(!!bupaR:::case_id_(log)) %>%
      #   mutate(#!!bupaR:::case_id_(log),
      #     prefix = purrr::accumulate(as.character(!!bupaR:::activity_id_(log)), paste, sep = " - "),
      #     #!!bupaR:::activity_id_(log),
      #     activity_duration = activity_duration
      #   ) %>%
      #   mutate(latest_time = activity_duration,
      #          #next_time = lead(activity_duration),
      #          recent_time = lag(activity_duration),
      #          recent_time = if_else(is.na(recent_time), 0, recent_time),
      #          remaining_time = last(time_passed) - time_passed[k+1]) %>%
      #   select(ith_case, !!bupaR:::case_id_(log), prefix, k, time_passed, recent_time, latest_time, remaining_time, activity_duration, trace, everything()) %>%
      #   re_map(mapping(log)) -> output
    #
    # }
    class(output) <- c("ppred_examples_df", class(output))

    attr(output, "task") <- task
    attr(output, "y_var") <- "remaining_time"
    attr(output, "features") <- c("throughput_time","processing_time","previous_duration")
    attr(output, "mapping") <- mapping(log)
    attr(output, "vocabulary") <- create_vocabulary(output)
  }

  # REMAINING_TRACE
  else if (task == "remaining_trace") {

    log %>%
      group_by(!!bupaR:::case_id_(log)) %>%
      mutate(ith_case = cur_group_id(),
             trace = paste(!!bupaR:::activity_id_(log), collapse = ","),
             k = row_number() - 1) %>% # getting the traces
      arrange(!!bupaR:::case_id_(log)) %>%
      distinct(!!bupaR:::activity_instance_id_(log), .keep_all = T) %>%
      mutate(
        prefix_list = purrr::accumulate(as.character(!!bupaR:::activity_id_(log)), c),
        prefix = purrr::accumulate(as.character(!!bupaR:::activity_id_(log)), paste, sep = " - ", .dir = "forward"),
        remaining_trace = purrr::accumulate(as.character(!!bupaR:::activity_id_(log)), paste, sep = " - ", .dir = "backward")) %>%
      mutate(remaining_trace = lead(remaining_trace),
             remaining_trace = if_else(is.na(remaining_trace), "endpoint", remaining_trace)) %>%
      select(ith_case, !!bupaR:::case_id_(log), prefix_list,
             prefix, remaining_trace, k, trace) -> output


    class(output) <- c("ppred_examples_df", class(output))

    attr(output, "task") <- task
    attr(output, "y_var") <- "remaining_trace"
    attr(output, "features") <- c()
    attr(output, "mapping") <- mapping(log)
    attr(output, "vocabulary") <- create_vocabulary(output)

  }
  return(output)
}

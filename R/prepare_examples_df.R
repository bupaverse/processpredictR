#' Title
#'
#' @param log
#' @param task
#' @param features
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
prepare_examples_dt <- function(log, task = c("outcome", "next_activity",
                                              "next_time", "remaining_time",
                                              "remaining_trace"), features = NULL, ...) {
  UseMethod("prepare_examples_dt")
}

#' @export
prepare_examples_dt.eventlog <- function(log, task = c("outcome", "next_activity",
                                                       "next_time", "remaining_time",
                                                       "remaining_trace"), features = NULL, ...) {


  task <- rlang::arg_match(task)

  log %>%
    select(AIID = activity_instance_id(log), any_of(features), force_df = T) %>%
    distinct() -> feature_data

  log_dt <- log %>%
    rename(AID = activity_id(log),
           CID = case_id(log),
           RID = resource_id(log),
           AIID = activity_instance_id(log),
           TS = timestamp(log)) %>%
    as.data.table() %>%
    mutate(AID = as.character(AID))

  log_dt[,
         .('start_time' = min(TS),
           'end_time' = max(TS),
           'min_order' = min(.order)),
         by = .(AID, AIID, CID, RID)][order(start_time, end_time, min_order),
                                      c("ith_case", "k","prefix_list", "remaining_trace_list") := .(
                                        .GRP,
                                        (1:.N)-1,
                                        Reduce(c, AID, accumulate = TRUE),
                                        shift(Reduce(c, AID, accumulate = TRUE, right = TRUE), fill = "endpoint", n = -1)
                                      ),
                                      .(CID)] -> new_log_dt

  if(task == "outcome") {

    new_log_dt[, c("outcome") := .(last(AID), ...), by = .(CID)] %>%
      mutate(outcome = forcats::fct_collapse(outcome, ...)) -> new_log_dt

    standard_features <- character()

  } else if(task == "next_activity") {

    new_log_dt[, c("next_activity") := .(shift(AID, n = -1, fill = "endpoint"))] -> new_log_dt

    standard_features <- character()
  } else if(task == "next_time") {

    new_log_dt[order(start_time, end_time, min_order),
               c("latest_duration",
                 "throughput_time",
                 "processing_time",
                 "time_before_activity",
                 "time_till_next_activity") := .(
                   as.double(end_time - start_time, units = "secs"),
                   as.double(end_time - min(start_time), units = "secs"),
                   cumsum(as.double(end_time - start_time, units = "secs")),
                   as.double(start_time - shift(end_time), units = "secs"),
                   as.double(shift(start_time, n = -1) - end_time, units = "secs")
                 ),
               .(CID)][,c("time_before_activity") := .(if_else(is.na(time_before_activity), 0, time_before_activity))] %>%
      filter(!is.na(time_till_next_activity)) -> new_log_dt
    standard_features <- c("latest_duration","throughput_time","processing_time","time_before_activity")

  } else if(task == "remaining_time") {

    new_log_dt[order(start_time, end_time, min_order),
               c("previous_duration",
                 "throughput_time",
                 "processing_time",
                 "remaining_time") := .(
                   shift(as.double(end_time - start_time, units = "secs")),
                   as.double(end_time - min(start_time), units = "secs"),
                   cumsum(as.double(end_time - start_time, units = "secs")),
                   as.double(last(end_time) - end_time, units = "secs")
                 ),
               .(CID)][,c("previous_duration") := .(if_else(is.na(previous_duration), 0, previous_duration))] -> new_log_dt

    standard_features <- c("previous_duration","throughput_time","processing_time")

  } else if(task == "remaining_trace") {

    # new_log_dt[order(start_time, end_time, min_order),
    #            c("remaining_trace_list") := Reduce(c, AID, accumulate = TRUE, right = TRUE), .(CID)] %>%
    new_log_dt %>%
      mutate(remaining_trace = map_chr(remaining_trace_list, paste, collapse = " - "),
             remaining_trace = if_else(is.na(remaining_trace), "endpoint", remaining_trace)) -> new_log_dt

    standard_features <- character()

  }

  y_var <- switch(task,
                  outcome = "outcome",
                  next_activity = "next_activity",
                  next_time = "time_till_next_activity",
                  remaining_time = "remaining_time",
                  remaining_trace = "remaining_trace")

  new_log_dt %>%
    merge(feature_data, by = "AIID") %>%
    as_tibble() %>%
    mutate(prefix = map_chr(prefix_list, paste, collapse = " - ")) %>%
    select(ith_case, !!bupaR:::case_id_(log) := CID, prefix, prefix_list, any_of(c(y_var, standard_features, features)), k, !!bupaR:::activity_id_(log) := AID, !!bupaR:::resource_id_(log) := RID,
           !!bupaR:::activity_instance_id_(log) := AIID, start_time, end_time, remaining_trace_list) -> output


  class(output) <- c("ppred_examples_df", class(output))

  attr(output, "task") <- task
  attr(output, "y_var") <- y_var
  attr(output, "features") <- c(standard_features, features)
  attr(output, "mapping") <- mapping(log)
  attr(output, "max_case_length") <- max_case_length(output)
  vocabulary <- create_vocabulary(output)
  attr(output, "vocab_size") <- vocabulary$keys_x %>% length() %>% as.integer()
  attr(output, "vocabulary") <- vocabulary

  return(output)

}

#' @export
prepare_examples_dt.activitylog <- function(log, task = c("outcome", "next_activity",
                                                          "next_time", "remaining_time",
                                                          "remaining_trace"), features = NULL, ...) {
}

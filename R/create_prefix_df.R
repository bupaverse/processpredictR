#' Create a dataframe with prefixes
#'
#' @param log An object of class log
#' @return a dataframe
#' @examples
#' create_prefix_df(patients)
#'
#' @export
create_prefix_df <- function(log) {
  UseMethod("create_prefix_df")
}

#' @export
create_prefix_df.log <- function(log) {
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
           last_activity = last(current_activity)) %>%
    ungroup()

  case_prefix

}

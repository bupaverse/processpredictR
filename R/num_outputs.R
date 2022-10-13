#' Calculate number of outputs (target variables)
#'
#' (WIP)
#'
#' @param processed_df A processed dataframe
#'
#' @return an integer number of outputs
#'
#' @export
num_outputs <- function(processed_df) {

  if ("outcome" %in% names(processed_df)) {

    processed_df$outcome %>% unique() %>% length()

  }

  else if ("next_activity" %in% names(processed_df)) {

    processed_df$next_activity %>% unique() %>% length()

  }

}

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

  task <- get_task(processed_df)
  processed_df[[task]] %>% unique() %>% length()

}

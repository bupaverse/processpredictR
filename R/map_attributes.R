#' Map attributes to a (custom) model
#'
#' @description Maps attributes and class [`"ppred_model"`] of a processed [`data.frame`] to a (custom) model.
#'
#' @inheritParams compile_model
#' @inheritParams create_model
#'
#'
#' @export
map_attributes <- function(model, processed_df) {

  # max case length
  maxlen <- max_case_length(processed_df)

  # number of features
  if (!is.null(attr(processed_df, "features"))) {
    num_features <- processed_df %>% attr("features") %>% length()
  }
  else {
    num_features <- 0
  }

  attr(model, "max_case_length") <- maxlen
  attr(model, "features") <- processed_df %>% attr("features")
  attr(model, "num_features") <- num_features
  class(model) <- c("ppred_model", class(model))
  return(model)
}

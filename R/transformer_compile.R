#' Compile and fit transformer model (test)
#'
#' (test)
#'
#' @param transformer_model A defined transformer model
#' @param learning_rate A learning rate of a model
#'
#' @export
transformer_compile <- function(transformer_model, learning_rate) {

  if (transformer_model$name == "outcome_OR_nextActivity_transformer") {
  source_python("inst/fit_model_outcome_OR_next_activity.py")
  compile_model(transformer_model, learning_rate)

  }

  else if (transformer_model$name == "next_time_transformer" || transformer_model$name == "remaining_time_transformer") {
  source_python("inst/fit_model_next_time.py")

  }

  compile_model(transformer_model, learning_rate)

}

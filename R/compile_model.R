#' Compile transformer model
#'
#'
#'
#' @param transformer_model A defined transformer model
#' @param learning_rate A learning rate of a model
#'
#' @export
compile_model <- function(transformer_model, learning_rate) {
  UseMethod("compile_model")
}

#' @export
compile_model.ppred_model <- function(transformer_model, learning_rate) {

  if (transformer_model$name %in% c("outcome", "next_activity", "remaining_trace")) {

    source_python("inst/fit_outcome_activity_trace.py")
  }

  else if (transformer_model$name %in% c("next_time", "remaining_time")) {

    source_python("inst/fit_time.py")
  }

  compile_model_py(transformer_model, learning_rate)
  message("Compilation complete!")
}

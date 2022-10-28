#' Compile and fit transformer model (test)
#'
#' (test)
#'
#' @param transformer_model A defined transformer model
#' @param learning_rate A learning rate of a model
#'
#' @export
transformer_compile <- function(transformer_model, learning_rate) {

  if (transformer_model$name == "outcome_OR_nextActivity_transformer" ||
      transformer_model$name == "remaining_trace_transformer") {

    source_python("inst/fit_outcome_activity_trace.py")

  }

  else if (transformer_model$name == "next_time_transformer" || transformer_model$name == "remaining_time_transformer") {
    source_python("inst/fit_time.py")

  }

  compile_model(transformer_model, learning_rate)

  print("Compilation complete!")

}

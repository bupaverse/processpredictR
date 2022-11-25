#' @title Default compile function for ProcessTransformer model
#' @param object Object of class [ppred_model]
#' @param ... Additional Arguments
#'
#' @export
compile_ProcessTransformer <- function(object, ...) {
  UseMethod("compile_ProcessTransformer")
}

#' @export
compile_ProcessTransformer.ppred_model <- function(object, ...) {

  if (attr(object, "task") %in% c("outcome", "next_activity", "remaining_trace")) {
    keras::compile(object,
                   optimizer = optimizer_adam(0.001),
                   loss = loss_sparse_categorical_crossentropy(from_logits = T),
                   metrics = metric_sparse_categorical_accuracy())
  }
  else {
    keras::compile(object,
                   optimizer = optimizer_adam(0.001),
                   loss = loss_logcosh())
  }
  message("Compilation complete!")
}

#' @title Default compile function for ProcessTransformer model
#' @param object Object of class [ppred_model]
#' @param ... Additional Arguments
#' @importFrom keras compile
#' @export
keras::compile


#' @export
compile.ppred_model <- function(object, ...) {

  if (object$task %in% c("outcome", "next_activity", "remaining_trace")) {
    keras::compile(object$model,
                   optimizer = optimizer_adam(0.001),
                   loss = loss_sparse_categorical_crossentropy(from_logits = T),
                   metrics = metric_sparse_categorical_accuracy())
  }
  else {
    keras::compile(object$model,
                   optimizer = optimizer_adam(0.001),
                   loss = loss_logcosh())
  }
  message("Compilation complete!")
}

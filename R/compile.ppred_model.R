#' @title Default compile function for ProcessTransformer model
#' @param object Object of class [ppred_model]
#' @param optimizer Default optimizer for ppred_model
#' @param loss Default loss for ppred_model
#' @param metrics Default metrics for ppred_model
#' @param ... Additional Arguments
#' @importFrom keras compile
#' @export
keras::compile


#' @export
compile.ppred_model <- function(object,
                                optimizer = optimizer_adam(0.001),
                                loss = if(object$task %in% c("outcome", "next_activity", "remaining_trace")) loss_sparse_categorical_crossentropy(from_logits = T) else loss_logcosh(),
                                metrics = if(object$task %in% c("outcome", "next_activity", "remaining_trace")) metric_sparse_categorical_accuracy() else list(keras::metric_mean_absolute_error(),
                                                                                                                                                               keras::metric_mean_squared_error(),
                                                                                                                                                               keras::metric_root_mean_squared_error(),
                                                                                                                                                               keras::metric_logcosh_error()),
                                ...) {
  # Cross-Entropy = 0.00: Perfect probabilities.
  # Cross-Entropy < 0.02: Great probabilities.
  # Cross-Entropy < 0.05: On the right track.
  # Cross-Entropy < 0.20: Fine.
  # Cross-Entropy > 0.30: Not great.
  # Cross-Entropy > 1.00: Terrible.
  # Cross-Entropy > 2.00 Something is broken.

  # if (object$task %in% c("outcome", "next_activity", "remaining_trace")) {
    keras::compile(object$model,
                   optimizer = optimizer,
                   loss = loss,
                   metrics = metrics,
                   ...)

  message("Compilation complete!")
}

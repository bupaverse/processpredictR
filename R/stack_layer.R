#' Stacks a keras layer on top of existing model
#'
#' User friendly interface to add a keras layer on top of existing model.
#'
#' @export
stack_layer <- function(object, ...) {

  if (any(class(object) == "ppred_model")) {

    outputs <- object$model$output %>%
      eval(...)

    keras::keras_model(inputs = object$model$input, outputs = outputs)

  }
  else {
    outputs <- object$output %>%
      eval(...)
  }

}

# model$model$output %>%
#   keras::layer_dense(units = 32, activation = "relu") -> outputs
#
# keras::keras_model(inputs = model$model$input, outputs = outputs)

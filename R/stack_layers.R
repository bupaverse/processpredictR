#' Stacks a keras layer on top of existing model
#'
#' User friendly interface to add a keras layer on top of existing model.

#' @export
stack_layers <- function(object, ...) {
  UseMethod("stack_layers")
}
#' @export
stack_layers.ppred_model <- function(object, ...) {

  layers <- list(...)

  for(i in 1:length(layers)) {
    object <- stack_layer(object, layers[[i]])
  }

  return(object)
}

stack_layer <- function(object, layer) {

  outputs <- object$model$output %>% layer

  object$model <- keras::keras_model(inputs = object$model$input, outputs = outputs, name = object$model$name)
  return(object)

}

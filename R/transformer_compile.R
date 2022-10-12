#' Compile and fit transformer model (test)
#'
#' (test)
#'
#' @param transformer_model A defined transformer model
#' @param learning_rate A learning rate of a model
#'
#' @export
transformer_compile <- function(transformer_model, learning_rate) {

  #compile model
  source_python("inst/fit_model.py")
  compile_model(transformer_model, learning_rate)

}

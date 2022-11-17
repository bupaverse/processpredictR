#' Fit model (test)
#'
#' (test)
#'
#' @param transformer_model A defined transformer model
#' @param tokens_train A list of train tokens, i.e. token_x, token_y
#' @param num_epochs  A number of epochs
#' @param batch_size A batch size
#' @param file Name of saved model (weights)
#'
#' @export
fit_model <- function(transformer_model, tokens_train, num_epochs, batch_size, file) {
  UseMethod("fit_model")
}
#' @export
fit_model.ppred_model <- function(transformer_model, tokens_train, num_epochs, batch_size, file) {

  # max_case_length
  maxlen <- attr(transformer_model, "max_case_length") %>% as.integer()

  # same for all tasks
  train_token_x <- tokens_train$token_x %>% keras::pad_sequences(maxlen = maxlen, value = 0)
  train_token_x <- train_token_x %>% reticulate::np_array(dtype = "float32")
  train_token_y <- tokens_train$token_y  %>% reticulate::np_array(dtype = "float32")

  num_epochs <- num_epochs %>% as.integer()
  batch_size <- batch_size %>% as.integer()

  # features
  if (attr(model, "num_features") > 0) {
  train_time_x <- matrix(attr(model, "features"), ncol = attr(model, "num_features")) %>%
    reticulate::np_array(dtype = "float32")

  source_python("inst/fit_time.py")
  fit_model_py(transformer_model, train_token_x, train_features_x, train_token_y, num_epochs, batch_size, file)

  }

  else {
      source_python("inst/fit_outcome_activity_trace.py")
      fit_model_py(transformer_model, train_token_x, train_token_y, num_epochs, batch_size, file)
  }

  # if (transformer_model$name %in% c("outcome", "next_activity", "remaining_trace")) {
  #
  #   source_python("inst/fit_outcome_activity_trace.py")
  #   fit_model_py(transformer_model, train_token_x, train_token_y, num_epochs, batch_size, file)
  #
  # }

  # else if (transformer_model$name %in% c("next_time", "remaining_time")) {
  #
  #   source_python("inst/fit_time.py")
  #   fit_model_py(transformer_model, train_token_x, train_features_x, train_token_y, num_epochs, batch_size, file)
  #
  # }

}


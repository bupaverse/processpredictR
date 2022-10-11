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
fit_test <- function(transformer_model, tokens_train, num_epochs, batch_size, file) {
  train_token_x <- tokens_train$token_x %>% keras::pad_sequences(maxlen = max_case_length(patients), value = 0)
  train_token_x <- train_token_x %>% reticulate::np_array(dtype = "float32")
  train_token_y <- tokens_train$token_y  %>% reticulate::np_array(dtype = "float32")

  source_python("inst/fit_model.py")
  fit_model(transformer_model, train_token_x, train_token_y, num_epochs, batch_size, file)

}

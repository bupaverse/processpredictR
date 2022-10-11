#' Compile and fit transformer model
#'
#' (WIP)
#'
#' @param transformer_model A defined transformer model
#' @param tokens_train A list of train tokens, i.e. token_x, token_y
#' @param learning_rate A learning rate of a model
#' @param num_epochs  A number of epochs
#' @param batch_size A batch size
#' @param file Name of saved model (weights)
#'
#' @export
compile_fit_model <- function(transformer_model, tokens_train, learning_rate, num_epochs, batch_size, file) {

  #compile model
  source_python("inst/fit_model.py")
  compile_model(transformer_model, learning_rate)

  #fit model
  train_token_x <- tokens_train$token_x %>% keras::pad_sequences(maxlen = max_case_length(patients), value = 0)
  train_token_x <- train_token_x %>% np_array(dtype = "float32")
  train_token_y <- tokens_train$token_y  %>% np_array(dtype = "float32")

  fit_model(transformer_model, train_token_x, train_token_y, num_epochs, batch_size, file)

}

# compile_fit_model(test, train_token_x = train_token_x, train_token_y = train_token_y, learning_rate = 0.001,
#                   num_epochs = as.integer(10), batch_size = as.integer(12), file = "test_model")

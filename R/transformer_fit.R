#' Fit model (test)
#'
#' (test)
#'
#' @param transformer_model A defined transformer model
#' @param tokens_train A list of train tokens, i.e. token_x, token_y
#' @param maxlen An integer number of the maximum case length (longest trace) in an event log
#' @param num_epochs  A number of epochs
#' @param batch_size A batch size
#' @param file Name of saved model (weights)
#'
#' @export
transformer_fit <- function(transformer_model, tokens_train, maxlen, num_epochs, batch_size, file) {

  # same for all tasks
  train_token_x <- tokens_train$token_x %>% keras::pad_sequences(maxlen = maxlen, value = 0)
  train_token_x <- train_token_x %>% reticulate::np_array(dtype = "float32")
  train_token_y <- tokens_train$token_y  %>% reticulate::np_array(dtype = "float32")

  num_epochs <- num_epochs %>% as.integer()
  batch_size <- batch_size %>% as.integer()

  if (transformer_model$name == "outcome_OR_nextActivity_transformer" ||
      transformer_model$name == "remaining_trace_transformer") {

    source_python("inst/fit_outcome_activity_trace.py")
    fit_model(transformer_model, train_token_x, train_token_y, num_epochs, batch_size, file)

  }

  else if (transformer_model$name == "next_time_transformer" || transformer_model$name == "remaining_time_transformer") {

    train_time_x <- matrix(c(tokens_train$time_x$recent_time, tokens_train$time_x$latest_time, tokens_train$time_x$time_passed), ncol = 3) %>%
      reticulate::np_array(dtype = "float32")

    source_python("inst/fit_time.py")
    fit_model(transformer_model, train_token_x, train_time_x, train_token_y, num_epochs, batch_size, file)

  }

}


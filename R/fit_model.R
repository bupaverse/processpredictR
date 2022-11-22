#' Fit model (test)
#'
#' (test)
#'
#' @param transformer_model A defined transformer model
#' @param train_data A training dataset
#' @param num_epochs  A number of epochs
#' @param batch_size A batch size
#' @param output_folder Name of saved model (weights)
#'
#' @export
fit_model <- function(transformer_model, train_data, num_epochs, batch_size, output_folder) {
  UseMethod("fit_model")
}
#' @export
fit_model.ppred_model <- function(transformer_model, train_data, num_epochs, batch_size, output_folder) {

  # max_case_length
  maxlen <- attr(transformer_model, "max_case_length") %>% as.integer()
  num_epochs <- num_epochs %>% as.integer()
  batch_size <- batch_size %>% as.integer()

  # tokenizing activities from the train dataset
  tokens_train <- tokenize(train_data)

  # same for all tasks
  train_token_x <- tokens_train$token_x %>% keras::pad_sequences(maxlen = maxlen, value = 0)
  train_token_x <- train_token_x %>% reticulate::np_array(dtype = "float32")
  train_token_y <- tokens_train$token_y  %>% reticulate::np_array(dtype = "float32")

  # features
  if (attr(transformer_model, "num_features") > 0) {
    train_features_x <- tokens_train$time_x %>%
      reticulate::np_array(dtype = "float32")

    #feats %>% purrr::map_if(is.numeric, scale) %>% as_tibble() %>% data.matrix

    # train_features_x <- matrix(feats, ncol = attr(transformer_model, "num_features")) %>%
    # reticulate::np_array(dtype = "float32")

    source_python("inst/fit_time.py")
    fit_model_py(transformer_model, train_token_x, train_features_x, train_token_y, num_epochs, batch_size, output_folder)

  }

  else {
      source_python("inst/fit_outcome_activity_trace.py")
      fit_model_py(transformer_model, train_token_x, train_token_y, num_epochs, batch_size, output_folder)
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


#' @title Default fit function for ProcessTransformer model
#' @param object Object of class [ppred_model]: A transformer model
#' @param train_data A training dataset
#' @param batch_size A batch size
#' @param num_epochs  A number of epochs
#' @param verbose A verbose
#' @param callbacks [`list`]: A list of callbacks
#' @param shuffle [`logical`] (default [`TRUE`]): If [`TRUE`] shuffles the data
#' @param validation_split A ratio to split on
#' @param ... Additional Arguments
#'
#' @export
fit_ProcessTransformer <- function(object, train_data,
                                   batch_size = batch_size,
                                   epochs = epochs,
                                   verbose = 1,
                                   callbacks = callbacks,
                                   shuffle = TRUE,
                                   validation_split = 0.2, ...) {
  UseMethod("fit_ProcessTransformer")
}

#' @export
fit_ProcessTransformer.ppred_model <- function(object, train_data,
                                               batch_size = batch_size,
                                               epochs = epochs,
                                               verbose = 1,
                                               callbacks = list(keras::callback_csv_logger(filename = "fit_outcome"),
                                                                            keras::callback_tensorboard()),
                                               shuffle = TRUE,
                                               validation_split = 0.2, ...) {

  # max_case_length
  maxlen <- attr(object, "max_case_length") %>% as.integer()
  num_epochs <- epochs %>% as.integer()
  batch_size <- batch_size %>% as.integer()

  # tokenizing activities from the train dataset
  tokens_train <- tokenize(train_data)

  # same for all tasks
  x_token_train <- tokens_train$token_x %>% keras::pad_sequences(maxlen = maxlen, value = 0)
  x_token_train <- x_token_train #%>% reticulate::np_array(dtype = "float32")
  y_token_train <- tokens_train$token_y #%>% reticulate::np_array(dtype = "float32")

  # keras::fit with extra features
  if (attr(object, "number_features") > 0) {
    x_features_train <- tokens_train$time_x

    keras::fit(object,
               x = list(x_token_train, x_features_train),
               y = y_token_train,
               batch_size = batch_size,
               epochs = epochs,
               verbose = verbose,
               callbacks = callbacks,
               shuffle = shuffle,
               validation_split = validation_split
               )
  }
  else {
    keras::fit(object,
               x = x_token_train,
               y = y_token_train,
               batch_size = batch_size,
               epochs = epochs,
               verbose = verbose,
               callbacks = callbacks,
               shuffle = shuffle,
               validation_split = validation_split
               )
  }
}

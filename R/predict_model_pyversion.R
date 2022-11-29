#' Predict model
#'
#' (WIP)
#'
#' @param transformer_model A defined transformer model.
#' @param test_data A test dataset.
#' @param metrics [`logical`] (default [`FALSE`]): Returns a vector of predicted values.
#'
#' @export
predict_model_pyversion <- function(transformer_model, test_data, metrics = FALSE) {
  UseMethod("predict_model")
}

#' @export
predict_model_pyversion.ppred_model <- function(transformer_model, test_data, metrics = FALSE) {

  # tokenizing activities from the train dataset
  tokens_test <- tokenize(test_data)

  # other parameters
  num_features <- attr(transformer_model, "num_features")
  maxlen <- attr(transformer_model, "max_case_length") %>% as.integer()
  test_token_x <- tokens_test$token_x %>% keras::pad_sequences(maxlen = maxlen, value = 0)
  test_token_x <- test_token_x %>% reticulate::np_array(dtype = "float32")
  test_token_y <- tokens_test$token_y  %>% reticulate::np_array(dtype = "float32")

  # predict_type: default predict function returns the predicted y
  if (metrics) {
    predict_type <- "metrics"
  }
  else {
    predict_type <- "y_pred"
  }

  source_python("inst/predict.py")

  if (num_features > 0) {

    test_time_x <- matrix(attr(transformer_model, "features"), ncol = num_features) %>%
      reticulate::np_array(dtype = "float32")

    output <- predict_model_next_time(transformer_model, test_token_x, test_time_x, test_token_y, predict_type)
    class(output) <- c("tokens_y_pred", class(output))
    return(output)
  }

  else {

    output <- predict_model(transformer_model, test_token_x, test_token_y, predict_type)
    class(output) <- c("tokens_y_pred", class(output))
    return(output)
  }
}


# predict_model(transformer_model = test, test_token_x = train_token_x, test_token_y = train_token_y)
# read_csv("results_model.csv")

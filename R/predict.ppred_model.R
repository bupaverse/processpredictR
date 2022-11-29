#' Generic predict function for ppred_model class.
#'
#' wip
#'
#' @param object [`ppred_model`] (default [`NULL`]): ProcessTransformer model of class [`ppred_model`].
#' @param test_data [`ppred_examples_df`] (default [`NULL`]): preprocessed test data.
#' @param ... Additional arguments
#'
#' @importFrom stats predict

#' @export
predict.ppred_model <- function(object, test_data, ...) {

  # if test_data is a preprocessed test dataset (before tokenize)
  if (any((test_data %>% class) == "ppred_examples_df")) {
    tokens_test <- test_data %>% tokenize()
    x_tokens_test <- tokens_test$token_x %>% keras::pad_sequences(maxlen = object$max_case_length, value = 0)
  }

  # if test_data is already tokenized
  else if (any((test_data %>% class) == "ppred_examples_tokens")) {
    tokens_test <- test_data
    x_tokens_test <- tokens_test$token_x %>% keras::pad_sequences(maxlen = object$max_case_length, value = 0)
  }

  if (object$number_features > 0) {
    x_features <- tokens_test$time_x
    y_pred <- stats::predict(object$model, list(x_tokens_test, x_features), ...) %>% keras::k_argmax(axis = -1) %>% as.integer()
  }

  else {
    y_pred <- stats::predict(object$model, x_tokens_test, ...) %>% keras::k_argmax(axis = -1) %>% as.integer()
  }
  return(y_pred)
}






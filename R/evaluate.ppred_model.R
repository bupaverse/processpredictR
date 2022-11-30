#' Generic evaluate function for ppred_model class.
#'
#' wip
#'
#' @param object [`ppred_model`] (default [`NULL`]): ProcessTransformer model of class [`ppred_model`].
#' @param test_data [`ppred_examples_df`] (default [`NULL`]): preprocessed test data.
#' @param ... Additional arguments
#'
#' @importFrom keras evaluate

#' @export
evaluate.ppred_model <- function(object, test_data, ...) {

  # if test_data is a preprocessed test dataset (before tokenize)
  if (any((test_data %>% class) == "ppred_examples_df")) {
    tokens_test <- test_data %>% tokenize()
}
  # if test_data is already tokenized
  else if (any((test_data %>% class) == "ppred_examples_tokens")) {
    tokens_test <- test_data
  }


  x_tokens_test <- tokens_test$token_x %>% keras::pad_sequences(maxlen = object$max_case_length, value = 0)
  y_tokens_test <- tokens_test$token_y

  # add extra features to a list of inputs
  if (object$number_features > 0) {
    x_features <- tokens_test$time_x
    results <- keras::evaluate(object$model, list(x_tokens_test, x_features), y_tokens_test, ...) %>% keras::k_argmax(axis = -1) %>% as.integer()
  }
  else {
    results <- keras::evaluate(object$model, x_tokens_test, y_tokens_test, ...) %>% keras::k_argmax(axis = -1) %>% as.integer()
  }

  return(results)
}






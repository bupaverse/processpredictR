#' @title Confusion matrix for
#' @param x [`ppred_predictions`] An event log with predicted values
#'
#' @export
confusion_matrix <- function(x, ...) {
  UseMethod("confusion_matrix")
}

#' @export
confusion_matrix.ppred_predictions <- function(x, as.tibble = F)  {

  # if test_data is a preprocessed test dataset (before tokenize)
  if (any((class(x)) == "ppred_predictions")) {
    output <- table(x$outcome, x$pred_label)
  }

  # if test_data is already tokenized
  else {
    simpleError("try again")
  }

  output

  # # if test_data is a preprocessed test dataset (before tokenize)
  # if (any((test_data %>% class) == "ppred_examples_df")) {
  #   tokens_test <- test_data %>% tokenize()
  #   y_tokens_test <- tokens_test$token_y
  # }
  #
  # # if test_data is already tokenized
  # else if (any((test_data %>% class) == "ppred_examples_tokens")) {
  #   tokens_test <- test_data
  #   y_tokens_test <- tokens_test$token_y
  # }

  # output <- table(as.numeric(y_pred), y_tokens_test)
}

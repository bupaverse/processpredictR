#' @title Confusion matrix for
#'
#' @param y_pred Predicted values
#' @param test_data Either a dataset or tokens
#' @param as.tibble [`logical`] (default [`FALSE`]): if [`TRUE`] returns a tibble
#'
#' @export
confusion_matrix <- function(y_pred, test_data, as.tibble = F)  {

  # if test_data is a preprocessed test dataset (before tokenize)
  if (any((test_data %>% class) == "ppred_examples_df")) {
    tokens_test <- test_data %>% tokenize()
    y_tokens_test <- tokens_test$token_y
  }

  # if test_data is already tokenized
  else if (any((test_data %>% class) == "ppred_examples_tokens")) {
    tokens_test <- test_data
    y_tokens_test <- tokens_test$token_y
  }

  output <- table(as.numeric(y_pred), y_tokens_test)

  if (!as.tibble) output
  else output %>% as_tibble()
}

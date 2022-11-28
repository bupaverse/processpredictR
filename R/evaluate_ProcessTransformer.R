#' Generic evaluate function for ppred_model class.
#'
#' wip
#'
#' @param object [`ppred_model`] (default [`NULL`]): ProcessTransformer model of class [`ppred_model`].
#' @param test_data [`ppred_examples_df`] (default [`NULL`]): preprocessed test data.
#' @param ... Additional arguments
#'
#' @export
evaluate_ProcessTransformer <- function(object, test_data, ...) {
  tokens_test <- test_data %>% tokenize()
  x_tokens_test <- tokens_test$token_x %>% keras::pad_sequences(maxlen = attr(object, "max_case_length"), value = 0)
  y_tokens_test <- tokens_test$token_y

  results <- keras::evaluate(model, x_tokens_test, y_tokens_test) %>% keras::k_argmax(axis = -1) %>% as.integer()

  return(results)

}






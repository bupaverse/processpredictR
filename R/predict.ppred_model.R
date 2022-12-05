#' Generic predict function for ppred_model class.
#' @param object [`ppred_model`] (default [`NULL`]): ProcessTransformer model of class [`ppred_model`].
#' @param test_data [`ppred_examples_df`] (default [`NULL`]): preprocessed test data.
#' @param append [`logical`] (default [`FALSE`]): if [`TRUE`], returns a passed [`data.frame`] with predicted values.
#' @param ... Additional arguments
#'
#' @importFrom stats predict
#' @export
stats::predict

#' @export
predict.ppred_model <- function(object, test_data, append = FALSE, ...) {

  # if test_data is a preprocessed test dataset (before tokenize)
  if (any((test_data %>% class) == "ppred_examples_df")) {
    tokens_test <- test_data %>% tokenize()
  }

  # if test_data is already tokenized
  else if (any((test_data %>% class) == "ppred_examples_tokens")) {
    tokens_test <- test_data
  }

  # tokenized trace
  x_tokens_test <- tokens_test$token_x %>% keras::pad_sequences(maxlen = object$max_case_length, value = 0)

  # if (object$number_features > 0) {
  x_numeric_features <- tokens_test$numeric_features
  x_categorical_features <- tokens_test$categorical_features

  x_test_list <- list(x_tokens_test)
  if (!is.null(x_numeric_features)) x_test_list <- x_test_list %>% append(list(x_numeric_features))
  if (!is.null(x_categorical_features)) x_test_list <- x_test_list %>% append(list(x_categorical_features))

  y_pred <- stats::predict(object$model, x_test_list, ...) #%>% keras::k_argmax(axis = -1)
  #model$model$get_layer("normalization_4")$mean ----> x_numeric_features mean



# y_pred postprocessing ---------------------------------------------------
  if (append) {
    # NEXT_TIME & REMAINING_TIME
    if (object$task %in% c("next_time", "remaining_time")) {
      mean <- object$y_normalize_layer$mean %>% as.double()
      variance <- object$y_normalize_layer$variance %>% as.double()
      y_pred <- y_pred * sqrt(variance) + mean

      if (any((test_data %>% class) == "ppred_examples_df")) {
        test_data %>% mutate(y_pred = as.numeric(y_pred)) %>%
          mutate(predicted_starttime_next_activity = complete + y_pred,
                 actual_starttime_next_activity = lead(start)) -> y_pred
        # as_tibble() %>% select(complete, actual_starttime_next_activity, predicted_starttime_next_activity) %>%
        # ggplotly(aes(predicted_starttime_next_activity, start2)) + geom_point()
      }
      else {
        tibble(y_pred = as.numeric(y_pred),
               y_actual = as.numeric(tokens_test$token_y)) -> y_pred
      }
    }

    # OUTCOME, NEXT_ACTIVITY & REMAINING_TRACE
    else if (object$task %in% c("outcome", "next_activity", "remaining_trace")) {
      y_pred <- y_pred %>% keras::k_argmax(axis = -1) %>% as.numeric()

      tibble(y_pred = as.numeric(y_pred),
             y_actual = as.numeric(tokens_test$token_y)) -> y_pred
    }
  }
  return(y_pred)
}






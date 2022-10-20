#' Predict model
#'
#' (WIP)
#'
#' @param transformer_model A defined transformer model
#' @param tokens_test A list of test tokens, i.e. token_x, token_y
#' @param maxlen An integer for longest trace
#' @param predict_type A type of output to generate c("y_pred", "metrics")
#'
#' @export
transformer_predict <- function(transformer_model, tokens_test, maxlen, predict_type = "metrics") {

  # same for all tasks
  test_token_x <- tokens_test$token_x %>% keras::pad_sequences(maxlen = maxlen, value = 0)
  test_token_x <- test_token_x %>% reticulate::np_array(dtype = "float32")
  test_token_y <- tokens_test$token_y  %>% reticulate::np_array(dtype = "float32")


  source_python("inst/predict.py")
  if (transformer_model$name == "outcome_OR_nextActivity_transformer") {

    predict_model(transformer_model, test_token_x, test_token_y, predict_type)

  }

  else if (transformer_model$name == "next_time_transformer") {

    test_time_x <- matrix(c(tokens_test$time_x$recent_time, tokens_test$time_x$latest_time, tokens_test$time_x$time_passed), ncol = 3) %>%
      reticulate::np_array(dtype = "float32")

    predict_model_next_time(transformer_model, test_token_x, test_time_x, test_token_y, predict_type)

  }

}


# predict_model(transformer_model = test, test_token_x = train_token_x, test_token_y = train_token_y)
# read_csv("results_model.csv")

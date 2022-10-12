#' Predict model
#'
#' (WIP)
#'
#' @param transformer_model A defined transformer model
#' @param tokens_test A list of test tokens, i.e. token_x, token_y
#'
#' @export
transformer_predict <- function(transformer_model, tokens_test) {

  test_token_x <- tokens_test$token_x %>% keras::pad_sequences(maxlen = maxlen, value = 0)
  test_token_x <- test_token_x %>% reticulate::np_array(dtype = "float32")
  test_token_y <- tokens_test$token_y  %>% reticulate::np_array(dtype = "float32")

  source_python("inst/predict.py")
  predict_model(transformer_model, test_token_x, test_token_y)

}


# predict_model(transformer_model = test, test_token_x = train_token_x, test_token_y = train_token_y)
# read_csv("results_model.csv")

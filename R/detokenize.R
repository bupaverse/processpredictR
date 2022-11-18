#' Detokenize predicted values
#'
#' Transforms predicted y_values back from tokens to labels
#'
#' @inheritParams tokenize
#' @param y_pred [`array`] A vector of predicted values.
#' @param y_actual [`array`] A vector of actual values.
#'
#' @export
detokenize <- function(processed_df, y_pred, y_actual) {
  UseMethod("detokenize")
}
#' @export
detokenize.tokens_y_pred <- function(processed_df, y_pred, y_actual) {

# back from tokens to labels
y_pred <- y_pred + 1
vocabulary <- get_vocabulary(x)
vocabulary <- vocabulary$keys_y %>%
  unlist() %>% as_tibble() %>% mutate(token = row_number())

y_pred %>% as_tibble() %>% rename(token = "value") %>% mutate(token = as.integer(token)) %>%
  left_join(vocabulary, by = "token") %>%
  mutate(y_actual = tokens_test$token_y + 1)
}

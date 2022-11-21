#' Detokenize predicted values
#'
#' Transforms predicted y_values back from tokens to labels
#'
#' @inheritParams tokenize
#' @param y_pred [`array`] An array of predicted values.
#' @param y_actual [`array`] (default [`NULL`]) An array of actual values.
#'
#' @export
detokenize <- function(y_pred, y_actual = NULL, processed_df) {
  UseMethod("detokenize")
}

#' @export
detokenize.tokens_y_pred <- function(y_pred, y_actual = NULL, processed_df) {

  y_pred <- y_pred + 1
  vocabulary <- get_vocabulary(processed_df)
  vocabulary <- vocabulary$keys_y %>%
    unlist() %>% as_tibble() %>% mutate(token = row_number())

  output <- y_pred %>% as_tibble() %>% rename(token = "value") %>% mutate(token = as.integer(token)) %>%
    left_join(vocabulary, by = "token")

  if (!is.null(y_actual)) {
    output <- output %>%
      mutate(y_actual = y_actual + 1)
  }

  return(output)

}

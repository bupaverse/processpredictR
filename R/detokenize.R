#' Detokenize predicted values
#'
#' Transforms predicted y_values back from tokens to labels
#'
#' @param y_pred [`array`] An array of predicted values.
#' @param y_actual [`array`] (default [`NULL`]) An array of actual values.
#'
#' @export
detokenize <- function(y_pred, y_actual = NULL, vocabulary) {

  y_pred <- y_pred + 1
  vocabulary <- vocabulary$keys_y %>%
    unlist() %>% as_tibble() %>% mutate(token = row_number())

  output <- y_pred %>% as_tibble() %>% rename(y_pred = "value") %>% mutate(y_pred = as.integer(y_pred)) %>%
    left_join(vocabulary, by = c("y_pred" = "token"))

  if (!is.null(y_actual)) {
    output <- output %>%
      mutate(y_actual = y_actual + 1) %>%
      left_join(vocabulary, by = c("y_actual" = "token"))
  }

  return(output)

}

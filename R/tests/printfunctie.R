#' @title Generic print function for token
#' @description Generic print function for eventlog
#' @param x Object of class token
#' @param ... Additional Arguments
#'
#' @export
print.token <- function(x, ...) {

  if ("token_time" %in% class(x)) {

    num_tokens <- lengths(x)
    cat("Number of tokens: ", num_tokens, "\n")



  }

  # token_x
  else {
    num_tokens <- length(x)
    num_act <- x %>% unlist() %>% length()
    cat("Number of tokens in a list: ", num_tokens, "\n")
    cat("Total number of tokenized activities: ", num_act, "\n")


  }


}

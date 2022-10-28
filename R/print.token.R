#' @title Generic print function for tokens
#' @description Generic print function for tokens
#' @param x Object of class token.
#' @param ... Additional Arguments
#' @importFrom purrr map_chr
#'
#' @export
print.token <- function(x, ...) {

num_tokens <- length(x)
cat(" -> consists of ", num_tokens, " tokens", "\n")
cat("something else", "\n")

if
}

print.tokens_time <- function(x, ...) {
  num_inputs <- length(x)
  num_tokens <- lengths(x)

  cat("list of ", num_inputs, " scaled time durations with ", num_tokens, " tokens for ", names(x), ", respectively.")
}

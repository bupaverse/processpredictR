#' @title Generic print function for tokens
#' @description Generic print function for tokens
#' @param x Object of class token.
#' @param ... Additional Arguments
#' @importFrom purrr map_chr
#'
#' @export
print.tokens <- function(x, ...) {

#cat(" -> consists of ", purrr::map_chr(x, length),  " tokens", "\n")
  cat(" -> consists of ", length(x),  " features", "\n")
}

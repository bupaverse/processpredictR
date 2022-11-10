#' @title Generic print function for tokens
#' @description Generic print function for tokens
#' @param x Object of class token.
#' @param ... Additional Arguments
#' @importFrom purrr map_chr
#'
#' @export
print.tokens <- function(x, ...) {

#cat(" -> consists of ", purrr::map_chr(x, length),  " tokens", "\n")
  cat("Tokenized examples","\n")
  for(i in 1:length(x)) {
    cat(names(tokens_train)[[i]])
    if(!is.null(names(tokens_train[[i]]))) {
      cat(": ", names(tokens_train[[i]]))
    }
    cat("\n")
  }

}

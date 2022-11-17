#' @title Generic print function for tokens
#' @description Generic print function for tokens
#' @param x Object of class token.
#' @param ... Additional Arguments
#' @importFrom purrr map_chr
#'
#' @export
print.ppred_examples_tokens <- function(x, ...) {

#cat(" -> consists of ", purrr::map_chr(x, length),  " tokens", "\n")
  cat("Tokenized examples","\n")
  for(i in 1:length(x)) {
    cat(names(x)[[i]], "\n")
    if (is.list(x[[i]])) {
      print.default(head(x[[i]], 5))
    }

    if(!is.null(names(x[[i]]))) {
      cat(": ", names(x[[i]]), "\n")
    }
    cat("\n")
  }

}

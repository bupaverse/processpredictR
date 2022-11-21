


#' Utils
#'
#' @param examples
#'
#' @export
#'
get_vocabulary <- function(examples) {
  attr(examples, "vocabulary")
}

#' @export

get_task <- function(examples) {
  attr(examples, "task")
}

#' @export
hot_encode_feats <- function(examples) {
  mapping <- attr(examples, "mapping")
  features <- attr(examples, "features")
  feats <- examples %>% as_tibble() %>% select(features)

  cat_features <- feats %>% select(is.factor)
  cat_features %>%
    data.table::as.data.table() %>%
    mltools::one_hot(cols = names(cat_features)) %>% names -> names_hotencoded_features

  names_num_features <- feats %>% select(is.numeric) %>% names

  output <- examples %>%
    data.table::as.data.table() %>%
    mltools::one_hot(cols = names(cat_features), dropCols = F) %>%
    re_map(mapping = mapping)

  class(output) <- c("ppred_examples_df", class(output))
  attr(output, "features") <- names_num_features %>% append(names_hotencoded_features)
  attr(output, "numeric_features") <- names_num_features
  attr(output, "hot_encoded_categorical_features") <- names_hotencoded_features
  attr(output, "task") <- attr(examples, "task")
  attr(output, "y_var") <- attr(examples, "y_var")
  attr(output, "mapping") <- mapping
  attr(output, "vocabulary") <- attr(examples, "vocabulary")

  return(output)
}

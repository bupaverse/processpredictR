#' Define transformer model
#'
#' Defines the model using the keras functional API.
#'
#' @param df [`data.frame`]: A processed [`data.frame`] from prepare_examples
#' @param custom [`logical`] (default [`FALSE`]): If [`TRUE`], returns a custom model.
#' @param ... you can pass additional arguments to keras::keras_model (ex.: name)
#'
#' @export
create_model <- function(df, custom = FALSE, ...) {
  UseMethod("create_model")
}

#' @export
create_model.ppred_examples_df <- function(df, custom = FALSE, ...) {

  # parameters of the model
  task <- attr(df, "task")
  features <- attr(df, "features")
  num_features <- features %>% length() %>% as.integer()
  num_outputs <- num_outputs(df) %>% as.integer()
  max_case_length <- attr(df, "max_case_length") %>% as.integer()
  vocab_size <- attr(df, "vocab_size") %>% as.integer()
  embed_dim <- 36 %>% as.integer()
  num_heads <- 4 %>% as.integer()
  ff_dim <- 64 %>% as.integer()

  inputs <- keras::layer_input(shape = c(max_case_length))
  predictions <- inputs %>%
    TokenAndPositionEmbedding()(maxlen = max_case_length, vocab_size = vocab_size, embed_dim = embed_dim) %>%
    TransformerBlock()(embed_dim = embed_dim, num_heads = num_heads, ff_dim = ff_dim) %>%
    keras::layer_global_average_pooling_1d() #%>%
  # keras::layer_dropout(rate = 0.1) %>%
  # keras::layer_dense(units = 64, activation = 'relu') %>%
  # keras::layer_dropout(rate = 0.1) %>%
  # keras::layer_dense(units = num_outputs, activation = 'linear')

  if (num_features > 0) {
    extra_inputs <- keras::layer_input(shape = c(num_features))
    extra_predictions <- extra_inputs %>%
      keras::layer_dense(units = 32, activation = "relu")

    predictions <- keras::layer_concatenate(list(predictions, extra_predictions))
  }

  if (!custom) {
    predictions <- predictions %>%
      keras::layer_dropout(rate = 0.1) %>%
      keras::layer_dense(units = 64, activation = 'relu') %>%
      keras::layer_dropout(rate = 0.1) %>%
      keras::layer_dense(units = num_outputs, activation = 'linear')
  }

  if (num_features > 0) {
    model <- keras::keras_model(inputs = list(inputs, extra_inputs), outputs = predictions)
  }
  else {
    model <- keras::keras_model(inputs = inputs, outputs = predictions)
  }

 output <- list()
 class(output) <- c("ppred_model", class(output))
 output$model <- model
 # Attributes now stored as list-object components
 output$max_case_length <- max_case_length
 output$features <- features
 output$number_features <- num_features
 output$task <- task

 # Attributes for temporary backwards compatitibility
  attr(output, "max_case_length") <- max_case_length
  attr(output, "features") <- features
  attr(output, "number_features") <- num_features
  attr(output, "task") <- task

  output
}






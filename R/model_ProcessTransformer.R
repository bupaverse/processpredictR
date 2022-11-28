#' Define transformer model
#'
#' Defines the model using the keras functional API.
#'
#' @param df [`data.frame`]: A processed [`data.frame`] from prepare_examples
#' @param custom [`logical`] (default [`FALSE`]): If [`TRUE`], returns a custom model.
#' @param ... you can pass additional arguments to keras::keras_model (ex.: name)
#'
#' @export
model_ProcessTransformer <- function(df, custom = FALSE, ...) {
  UseMethod("model_ProcessTransformer")
}

#' @export
model_ProcessTransformer.ppred_examples_df <- function(df, custom = FALSE, ...) {

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

  class(model) <- c("ppred_model", class(model))
  attr(model, "max_case_length") <- max_case_length
  attr(model, "features") <- features
  attr(model, "number_features") <- num_features
  attr(model, "task") <- task


  model
}






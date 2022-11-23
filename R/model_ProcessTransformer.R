#' Define transformer model
#'
#' Defines the model using the keras functional API.
#'
#' @param df [`data.frame`]: A processed [`data.frame`] from prepare_examples
#'
#' @export
model_ProcessTransformer <- function(df, ...) {
  UseMethod("model_ProcessTransformer")
}

#' @export
model_ProcessTransformer.ppred_examples_df <- function(df, ...) {
  max_case_length <- max_case_length(df) %>% as.integer()
  vocab_size <- vocab_size(df) %>% as.integer()
  num_outputs <- num_outputs(df) %>% as.integer()
  embed_dim <- 36 %>% as.integer()
  num_heads <- 4 %>% as.integer()
  ff_dim <- 64 %>% as.integer()

  inputs <- keras::layer_input(shape = c(max_case_length))
  predictions <- inputs %>%
    TokenAndPositionEmbedding(maxlen = max_case_length, vocab_size = vocab_size, embed_dim = embed_dim) %>%
    TransformerBlock(embed_dim = embed_dim, num_heads = num_heads, ff_dim = ff_dim) %>%
    keras::layer_global_average_pooling_1d() %>%
    keras::layer_dropout(rate = 0.1) %>%
    keras::layer_dense(units = 64, activation = 'relu') %>%
    keras::layer_dropout(rate = 0.1) %>%
    keras::layer_dense(units = num_outputs, activation = 'linear')

  keras::keras_model(inputs = inputs, outputs = predictions) -> model
  model
}









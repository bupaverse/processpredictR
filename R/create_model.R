#' Define transformer model
#'
#' Defines the model using the keras functional API.
#'
#' @param x_train [`data.frame`]: A processed [`data.frame`] from prepare_examples
#' @param custom [`logical`] (default [`FALSE`]): If [`TRUE`], returns a custom model.
#' @param ... you can pass additional arguments to keras::keras_model (ex.: name)
#'
#' @export
create_model <- function(x_train, custom = FALSE, ...) {
  UseMethod("create_model")
}

#' @export
create_model.ppred_examples_df <- function(x_train, custom = FALSE, ...) {

  # parameters of the model
  task <- attr(x_train, "task")
  features <- attr(x_train, "features")
  num_features <- features %>% length() %>% as.integer()
  num_outputs <- attr(x_train, "num_outputs")
  max_case_length <- attr(x_train, "max_case_length") %>% as.integer()
  vocab_size <- attr(x_train, "vocab_size") %>% as.integer()
  embed_dim <- 36 %>% as.integer()
  num_heads <- 4 %>% as.integer()
  ff_dim <- 64 %>% as.integer()


# Initialize transformer model --------------------------------------------
  inputs <- keras::layer_input(shape = c(max_case_length))
  predictions <- inputs %>%
    TokenAndPositionEmbedding()(maxlen = max_case_length, vocab_size = vocab_size, embed_dim = embed_dim) %>%
    TransformerBlock()(embed_dim = embed_dim, num_heads = num_heads, ff_dim = ff_dim) %>%
    keras::layer_global_average_pooling_1d() #%>%
  # keras::layer_dropout(rate = 0.1) %>%
  # keras::layer_dense(units = 64, activation = 'relu') %>%
  # keras::layer_dropout(rate = 0.1) %>%
  # keras::layer_dense(units = num_outputs, activation = 'linear')

# extra features ----------------------------------------------------------
  numeric_features <- attr(x_train, "numeric_features")
  if (!is.null(numeric_features)) {
    numeric_features_train <- x_train %>%
      as_tibble() %>%
      select(numeric_features) %>% data.matrix()
  }

  if (num_features > 0) {
    extra_inputs <- keras::layer_input(shape = c(num_features))
    scale_numeric <- keras::layer_normalization()
    scale_numeric %>% adapt(numeric_features_train)
    extra_predictions <- extra_inputs %>%
      scale_numeric() %>%
      keras::layer_dense(units = 32, activation = "relu")

    predictions <- keras::layer_concatenate(list(predictions, extra_predictions))
  }

  # finalize default model
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


# assign attributes -------------------------------------------------------
 output <- list()
 class(output) <- c("ppred_model", class(output))
 output$model <- model
 # Attributes now stored as list-object components
 output$max_case_length <- max_case_length
 output$features <- features
 output$number_features <- num_features
 output$task <- task
 output$num_outputs <- num_outputs

 # Attributes for temporary backwards compatitibility
  attr(output, "max_case_length") <- max_case_length
  attr(output, "features") <- features
  attr(output, "number_features") <- num_features
  attr(output, "task") <- task

  output
}






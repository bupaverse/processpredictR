


#' Utils
#'
#' @param examples a preprocessed dataset returned by prepare_examples_dt().
#'
#'
get_vocabulary <- function(examples) {
  attr(examples, "vocabulary")
}

get_task <- function(examples) {
  attr(examples, "task")
}

hot_encode_feats <- function(examples) {
  mapping <- attr(examples, "mapping")
  features <- attr(examples, "features")

  # categorical features original and new hot-encoded features' names
  feats <- examples %>% as_tibble() %>% select(features)
  cat_features <- feats %>% select(where(is.factor))

  if (length(cat_features) > 0) {
    cat_features %>%
      data.table::as.data.table() %>%
      mltools::one_hot(cols = names(cat_features)) %>% names -> names_hotencoded_features

    output <- examples %>%
      data.table::as.data.table() %>%
      mltools::one_hot(cols = names(cat_features), dropCols = F) %>%
      as_tibble()
  }

  else {
    names_hotencoded_features <- NULL
    output <- examples
  }
  # names numeric features
  names_num_features <- feats %>% select(where(is.double)) %>% names
  if (length(names_num_features) == 0) names_num_features <- NULL


  class(output) <- c("ppred_examples_df", class(output))
  attr(output, "task") <- attr(examples, "task")
  attr(output, "y_var") <- attr(examples, "y_var")
  attr(output, "max_case_length") <- attr(examples, "max_case_length")
  attr(output, "vocab_size") <- attr(examples, "vocab_size")
  attr(output, "num_outputs") <- attr(examples, "num_outputs")


  attr(output, "numeric_features") <- names_num_features
  attr(output, "features") <- names_num_features %>% append(names_hotencoded_features)
  attr(output, "hot_encoded_categorical_features") <- names_hotencoded_features
  #attr(output, "number_features") <- names_num_features %>% append(names_hotencoded_features) %>% length

  attr(output, "mapping") <- mapping
  attr(output, "vocabulary") <- attr(examples, "vocabulary")

  return(output)
}

TransformerBlock <- function() {
  super <- NULL
  self <- NULL
  keras::new_layer_class(
    classname = "TransformerBlock",
    initialize = function(self, embed_dim, num_heads, ff_dim, rate = 0.1) {
      super$initialize()

      self$att <- keras::layer_multi_head_attention(num_heads=num_heads, key_dim=embed_dim)
      self$ffn <- keras::keras_model_sequential() %>%
        layer_dense(ff_dim, activation="relu") %>%
        layer_dense(embed_dim)

      self$layernorm_a <- keras::layer_layer_normalization(epsilon=1e-6)  #LayerNormalization(epsilon=1e-6)
      self$layernorm_b <- keras::layer_layer_normalization(epsilon=1e-6) # OF layer_layer_normalization
      self$dropout_a <- keras::layer_dropout(rate=0.1) #layers.Dropout(rate)
      self$dropout_b <- keras::layer_dropout(rate=0.1)
    },
    call = function(self, inputs, training) {
      attn_output <- self$att(inputs, inputs)
      attn_output <- self$dropout_a(attn_output, training=training)
      out_a <- self$layernorm_a(inputs + attn_output)
      ffn_output <- self$ffn(out_a)
      ffn_output <- self$dropout_b(ffn_output, training=training)
      return(self$layernorm_b(out_a + ffn_output))
    }
  )
}

# Input embeddings and positioning
# Neural networks learn through numbers so each word (=input) maps to a vector with continuous values to represent that word.
# The next step is to inject positional information into the embeddings. Because the transformer encoder has no recurrence
# like recurrent neural networks, we must add some information about the positions into the input embeddings.
# This is done using positional encoding. The following layer combines both the token embedding and it's positional embedding.
# The positional embedding is different from the original defined in the paper "Attention is all you need".
# Instead, `keras::layer_embedding()` is used, where the input dimension is set to the maximum case length (in an event log).
TokenAndPositionEmbedding  <- function() {
  super <- NULL
  self <- NULL
  keras::new_layer_class(
    classname = "TokenAndPositionEmbedding",
    initialize = function(self, maxlen, vocab_size, embed_dim, ...) {
      super$initialize()
      self$maxlen <- maxlen
      self$vocab_size <- vocab_size
      self$embed_dim <- embed_dim

      self$token_emb <- keras::layer_embedding(input_dim = vocab_size, output_dim = embed_dim) #layers.Embedding(input_dim=vocab_size, output_dim=embed_dim)
      self$pos_emb <- keras::layer_embedding(input_dim = maxlen, output_dim = embed_dim) #layers.Embedding(input_dim=maxlen, output_dim=embed_dim)

    },
    call = function(self, x) {
      maxlen <- tf$shape(x)[-1]  #tf.shape(x)[-1] NA, NULL, -1 is all the same
      positions <- tf$range(start=0, limit=maxlen, delta=1)
      positions <- self$pos_emb(positions)
      x <- self$token_emb(x)
      return(x + positions)
    },

    get_config = function() {
      # base_config <- super(TokenAndPositionEmbedding, self)$get_config()
      # list(maxlen = self$maxlen,
      #      vocab_size = self$vocab_size,
      #      embed_dim = self$embed_dim)

      # CUSTOM OBJECTS section (https://tensorflow.rstudio.com/guides/keras/serialization_and_saving)
      config <- super()$get_config()
      config$maxlen <- self$maxlen
      config$vocab_size <- self$vocab_size
      config$embed_dim <- self$embed_dim
      config
    }
  )
}

# Specifically for remaining_trace prediction. Here, `mask_zero` = TRUE and `d_model` instead of `embed_dim` notation for the layer_embedding's `output_dim`
TokenAndPositionEmbedding_RemainingTrace  <- function() {
  super <- NULL
  self <- NULL
  keras::new_layer_class(
    classname = "TokenAndPositionEmbedding",
    initialize = function(self, maxlen, vocab_size, d_model, ...) {
      super$initialize()
      self$maxlen <- maxlen
      self$vocab_size <- vocab_size
      self$d_model <- d_model

      self$token_emb <- keras::layer_embedding(input_dim = vocab_size, output_dim = d_model, mask_zero = TRUE) #layers.Embedding(input_dim=vocab_size, output_dim=embed_dim)
      self$pos_emb <- keras::layer_embedding(input_dim = maxlen, output_dim = d_model) #layers.Embedding(input_dim=maxlen, output_dim=embed_dim)

    },
    call = function(self, x) {
      maxlen <- tf$shape(x)[-1]  #tf.shape(x)[-1] NA, NULL, -1 is all the same
      positions <- tf$range(start=0, limit=maxlen, delta=1)
      positions <- self$pos_emb(positions)
      x <- self$token_emb(x)
      x <- x + positions
      return(x)
      #return(x + positions)
    },

    get_config = function() {
      # base_config <- super(TokenAndPositionEmbedding, self)$get_config()
      # list(maxlen = self$maxlen,
      #      vocab_size = self$vocab_size,
      #      embed_dim = self$embed_dim)

      # CUSTOM OBJECTS section (https://tensorflow.rstudio.com/guides/keras/serialization_and_saving)
      config <- super()$get_config()
      config$maxlen <- self$maxlen
      config$vocab_size <- self$vocab_size
      config$d_model <- self$d_model
      config
    }
  )
}


# Attention layers
# The base attention layer
# `BaseAttention` layer initializes multihead attention, normalization and add layer (in order to add a list of input tensors together).
# `BaseAttention` is a super class which will be consequently called by the other layers as methods (`GlobalSelfAttention`, `CrossAttention`, `CausalAttention`).
BaseAttention <- function() {
  super <- NULL
  self <- NULL
  keras::new_layer_class(
    classname = "BaseAttention",
    initialize = function(self, ...) {
      super$initialize()
      self$mha <- keras::layer_multi_head_attention(...) # **kwargs (key: value)
      self$layernorm <- keras::layer_normalization()
      self$add <- layer_add()
    }
  )
}

# The global self attention layer
# This layer is responsible for processing the context sequence, and propagating information along its length.
# Here, the target sequence `x` is passed as both `query` and `value` in the `mha` (multi-head attention) layer. This is self-attention.
GlobalSelfAttention <- function(baseattention = BaseAttention()) {
  GlobalSelfAttention(baseattention) %py_class% {
    call <- function(self, x) {
      attn_output <- self$mha(
        query=x,
        key=x,
        value=x
      )

      x <- self$add(list(x, attn_output))
      x <- self$layernorm(x)

      return(x)
    }
  }
}

# The cross attention layer
# At the literal center of the transformer network is the cross-attention layer. This layer connects the encoder and decoder.
# As opposed to `GlobalSelfAttention` layer, in the `CrossAttention` layer a target sequence `x` is passed as a `query`,
# whereas the attended/learned representations `context` by the encoder (described later) is passed as `key` and `value` in the `mha` (multi-head attention) layer.
CrossAttention <- function(baseattention = BaseAttention()) {
  CrossAttention(baseattention) %py_class% {
    call <- function(self, x, context) {
      attn_output <- self$mha(
        query=x,
        key=context,
        value=context, return_attention_scores=FALSE ######### SHOULD IT BE TRUE? DOCUMENTATION??
      )

      x <- self$add(list(x, attn_output))
      x <- self$layernorm(x)

      return(x)
    }
  }
}

# The causal self attention layer
# This layer does a similar job as the `GlobalSelfAttention` layer, for the output sequence.
# This needs to be handled differently from the encoder's `GlobalSelfAttention` layer.
# The causal mask ensures that each location only has access to the locations that come before it.
# The `attention_mask` argument is set to `True`. Thus, the output for early sequence elements doesn't depend on later elements.
CausalSelfAttention <- function(baseattention = BaseAttention()) {
  CausalSelfAttention(baseattention) %py_class% {
    call <- function(self, x) {
      attn_output <- self$mha(
        query=x,
        key=x,
        value=x,
        use_causal_mask = TRUE
      )

      x <- self$add(list(x, attn_output))
      x <- self$layernorm(x)

      return(x)
    }
  }
}

# The feed forward network
# The transformer also includes this point-wise feed-forward network in both the encoder and decoder.
# The network consists of two linear layers (tf.keras.layers.Dense) with a ReLU activation in-between, and a dropout layer.
# As with the attention layers the code here also includes the residual connection and normalization.
FeedForward <- function(keraslayersLayer = keras$layers$Layer) {
  super <- NULL
  self <- NULL
  FeedForward(keraslayersLayer) %py_class% {
    initialize <- function(self, d_model, dff, dropout_rate = 0.1) { # dff = ff_dim, d_model = embed_dim, dropout_rate = rate
      super$initialize()
      self$seq <- keras::keras_model_sequential() %>%
        layer_dense(dff, activation="relu") %>%
        layer_dense(d_model) %>%
        layer_dropout(rate = dropout_rate)

      self$add <- keras::layer_add()
      self$layer_norm <- keras::layer_normalization()
    }

    call <- function(self, x) {
      x <- self$add(list(x, self$seq(x)))
      x <- self$layer_norm(x)
      return(x)
    }
  }
}

# The encoder layer (= TransformerBlock in ProcessTransformer)
# Now we have the encoder layer. The encoder layer's job is to map all input sequences
# into an abstract continuous representation that holds the learned information for that entire sequence.
# It contains 2 sub-modules: multi-headed attention, followed by a fully connected network (`GlobalSelfAttention` and `FeedForward` layer, respectively).
# There are also residual connections around each of the two sublayers followed by a layer normalization.
# The encoder layer using new_class_layer wrapper (for using with pipe `%>%`):
EncoderLayer <- function() {
  super <- NULL
  self <- NULL
  new_layer_class(
    classname = "EncoderLayer",
    initialize = function(self, d_model, num_heads, dff, dropout_rate = 0.1, ...) { # dff = ff_dim, d_model = embed_dim, dropout_rate = rate
      super$initialize()

      self$self_attention <- GlobalSelfAttention()(num_heads = num_heads,
                                                 key_dim = d_model,
                                                 dropout = dropout_rate
      )
      self$ffn <- FeedForward()(d_model, dff)
    },

    call = function(self, x) {
      x <- self$self_attention(x)
      x <- self$ffn(x)
      return(x)
    }
  )
}


# The decoder layer
# The decoder's stack is slightly more complex, with each `DecoderLayer` containing a `CausalSelfAttention`, a `CrossAttention`, and a `FeedForward` layer.
# The decoder layer using new_class_layer wrapper (for using with pipe `%>%`):
DecoderLayer <- function() {
  super <- NULL
  self <- NULL
  keras::new_layer_class(
    classname = "DecoderLayer",
    initialize = function(self, d_model, num_heads, dff, dropout_rate = 0.1, ...) { # dff = ff_dim, d_model = embed_dim, dropout_rate = rate
      super$initialize()

      self$causal_self_attention <- CausalSelfAttention()(
        num_heads=num_heads,
        key_dim=d_model,
        dropout=dropout_rate)

      self$cross_attention <- CrossAttention()(
        num_heads=num_heads,
        key_dim=d_model,
        dropout=dropout_rate)

      self$ffn <- FeedForward()(d_model, dff)
    },

    #self$final_layer <- keras::layer_dense(vocab_size) # or maxlen?

    call = function(x, context) {

      x <- self$causal_self_attention(x=x)
      x <- self$cross_attention(x=x, context=context)

      # # Cache the last attention scores for plotting later
      # self$last_attn_scores = self.cross_attention.last_attn_scores

      x <- self$ffn(x)  # Shape `(batch_size, seq_len, d_model)`.

      #x <- self$final_layer(x)
      return(x)
    }
  )
}


# function for transforming remaining_trace to remaining_trace_s2s --------
prep_remaining_trace2 <- function(log) {
  log$remaining_trace_list <- log$remaining_trace_list %>% purrr::map(~append("startpoint", .))
  vocabulary <- log %>% attr("vocabulary")
  vocabulary$keys_x <- vocabulary$keys_x %>% append(list("endpoint")) %>% append(list("startpoint"))

  # prefix_list to tokens
  token_x <- list()
  for (i in (1:nrow(log))) {
    #case_trace <- list()
    case_trace <- c()

    for (j in (1:length(log$prefix_list[[i]]))) {
      #if (processed_log$trace[[i]][j] == x_word_dict$values_x) {}
      tok <- which(log$prefix_list[[i]][j] == vocabulary$keys_x)
      case_trace <- case_trace %>% append(tok-1)
    }

    case_trace <- case_trace %>% list()
    token_x <- token_x %>% append(case_trace)
  }

  # remaining_trace_list to tokens
  token_y <- list()
  for (i in (1:nrow(log))) {
    #case_trace <- list()
    case_trace <- c()

    for (j in (1:length(log$remaining_trace_list[[i]]))) {
      #if (processed_log$trace[[i]][j] == x_word_dict$values_x) {}
      tok <- which(log$remaining_trace_list[[i]][j] == vocabulary$keys_x)
      case_trace <- case_trace %>% append(tok-1)
    }

    case_trace <- case_trace %>% list()
    token_y <- token_y %>% append(case_trace)
  }

  # shift remaining_tokens for training
  remaining_tokens_shifted <- token_y %>% purrr::map_depth(.depth = 1, lead, n=1, default = 0)
  remaining_tokens_shifted <- remaining_tokens_shifted %>% keras::pad_sequences(padding = "post")
  # remaining_tokens
  remaining_tokens <- token_y %>% keras::pad_sequences(padding = "post")
  # current_tokens
  current_tokens <- token_x %>% keras::pad_sequences()



  # # remaining trace list containing tokenized sequences with some mapped attributes
  # remaining_trace <- list(log = log, current_tokens = current_tokens, remaining_tokens = remaining_tokens,
  #                        remaining_tokens_shifted = remaining_tokens_shifted)
  remaining_trace <- list()
  class(remaining_trace) <- c("remaining_trace_s2s", class(remaining_trace))
  remaining_trace$log <- log
  remaining_trace$current_tokens <- current_tokens
  remaining_trace$remaining_tokens <- remaining_tokens
  remaining_trace$remaining_tokens_shifted <- remaining_tokens_shifted

  # attributes to each tokenized sequence
  attr(remaining_trace, "task") <- "remaining_trace_s2s"
  attr(remaining_trace, "vocabulary") <- vocabulary$keys_x
  attr(remaining_trace, "vocab_size") <- vocabulary$keys_x %>% length()
  attr(remaining_trace, "max_case_length") <- log %>% attr("max_case_length")
  attr(remaining_trace, "input_maxlen") <- log %>% attr("max_case_length") #remaining_trace$current_tokens %>% ncol() ## NOTE max_case_length(log) is false because would be based on a new split data
  attr(remaining_trace, "target_maxlen") <- log %>% attr("max_case_length") + 1 # only "endpoint" can take place in the remaining_trace_list

  return(remaining_trace)
}

# bindings for global variable
output_dim <- NULL
dim_ff <- NULL
embed_dim <- NULL
ff_dim <- NULL

# Create models -----------------------------------------------------------
# model from ProcessTransformer Python library
create_model_original <- function(x_train, custom = custom, num_heads, embed_dim, ff_dim, ...) {

  # parameters of the model
  task <- attr(x_train, "task")
  features <- attr(x_train, "features")
  num_features <- features %>% length() %>% as.integer()
  numeric_features <- attr(x_train, "numeric_features")
  categorical_features <- attr(x_train, "hot_encoded_categorical_features")
  # number_numeric_features <- attr(x_train, "numeric_features") %>% length() %>% as.integer()
  # number_categorical_features <- attr(x_train, "hot_encoded_categorical_features") %>% length() %>% as.integer()
  num_outputs <- attr(x_train, "num_outputs")
  max_case_length <- attr(x_train, "max_case_length") %>% as.integer()
  vocab_size <- attr(x_train, "vocab_size") %>% as.integer()
  # embed_dim <- 36 %>% as.integer()
  # num_heads <- 4 %>% as.integer()
  # ff_dim <- 64 %>% as.integer()


  # Initialize transformer model --------------------------------------------
  inputs <- keras::layer_input(shape = c(max_case_length))
  outputs <- inputs %>%
    TokenAndPositionEmbedding()(maxlen = max_case_length, vocab_size = vocab_size, embed_dim = embed_dim) %>%
    TransformerBlock()(embed_dim = embed_dim, num_heads = num_heads, ff_dim = ff_dim) %>%
    keras::layer_global_average_pooling_1d() #%>%
  # keras::layer_dropout(rate = 0.1) %>%
  # keras::layer_dense(units = 64, activation = 'relu') %>%
  # keras::layer_dropout(rate = 0.1) %>%
  # keras::layer_dense(units = num_outputs, activation = 'linear')

  # extra features ----------------------------------------------------------
  if (!is.null(numeric_features)) {
    numeric_features_train <- x_train %>%
      as_tibble() %>%
      select(numeric_features) %>% data.matrix()

    #}

    #if (number_numeric_features > 0) {
    #number_numeric_features <- numeric_features %>% length() %>% as.integer()
    number_numeric_features <- numeric_features_train %>% colnames() %>% length() %>% as.integer()

    numeric_inputs <- keras::layer_input(shape = c(number_numeric_features))
    scale_numeric <- keras::layer_normalization()
    scale_numeric %>% adapt(numeric_features_train)
    numeric_outputs <- numeric_inputs %>%
      scale_numeric() %>%
      keras::layer_dense(units = 32, activation = "relu")

    outputs <- keras::layer_concatenate(list(outputs, numeric_outputs))
  }
  else numeric_inputs <- NULL

  #if (number_categorical_features > 0) {
  if (!is.null(categorical_features)) {
    categorical_features_train <- x_train %>%
      as_tibble() %>%
      select(categorical_features) %>% data.matrix()

    #number_categorical_features <- categorical_features %>% length() %>% as.integer()
    number_categorical_features <- categorical_features_train %>% colnames() %>% length() %>% as.integer()
    categorical_inputs <- keras::layer_input(shape = c(number_categorical_features))
    categorical_outputs <- categorical_inputs %>%
      keras::layer_dense(units = 32, activation = "relu") # change units ????

    outputs <- keras::layer_concatenate(list(outputs, categorical_outputs))
  }
  else categorical_inputs <- NULL


  # finalize default model --------------------------------------------------
  if (!custom) {
    outputs <- outputs %>%
      keras::layer_dropout(rate = 0.1) %>%
      keras::layer_dense(units = 64, activation = 'relu') %>%
      keras::layer_dropout(rate = 0.1) %>%
      keras::layer_dense(units = num_outputs, activation = if_else(task %in% c("next_time", "remaining_time"), "relu", "softmax"))
  }

  #if (num_features > 0) {
  list_inputs <- list(inputs) %>% append(numeric_inputs) %>% append(categorical_inputs)
  model <- keras::keras_model(inputs = list_inputs, outputs = outputs, ...)
  #}
  #else {
  #model <- keras::keras_model(inputs = inputs, outputs = outputs)
  #}


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
  output$numeric_features <- numeric_features
  output$categorical_features <- categorical_features
  output$vocabulary <- attr(x_train, "vocabulary")

  if (task %in% c("next_time", "remaining_time")) {

    y_token_train <- x_train %>%
      as_tibble() %>%
      pull(attr(x_train, "y_var")) #%>% data.matrix()

    #######################   #######################   #######################   #######################

    sd_time <- sd(y_token_train)
    output$sd_time <- sd_time

    # y_token_train <- x_train %>%
    #   as_tibble() %>%
    #   select(attr(x_train, "y_var")) %>% data.matrix() FOR OUTCOME WAS VERY INTERESTING

    # # original
    # normalize_y <- keras::layer_normalization()
    # normalize_y %>% adapt(y_token_train)
    # output$y_normalize_layer <- normalize_y
  }

  #######################   #######################   #######################   #######################

  # Attributes for temporary backwards compatitibility
  attr(output, "max_case_length") <- max_case_length
  attr(output, "features") <- features
  attr(output, "number_features") <- num_features
  attr(output, "task") <- task

  output
}

# s2s model with both encoder and decoder
create_model_s2s <- function(x_train, num_heads = num_heads, d_model = output_dim, dff = dim_ff, ...) {

  # # Parameters of the model
  # d_model <- 128
  # dff <- 512
  # num_heads <- 8
  dropout_rate <- 0.1
  vocab_size <- x_train %>% attr("vocab_size")
  input_maxlen <- x_train %>% attr("input_maxlen")
  target_maxlen <- x_train %>% attr("target_maxlen")

  # NB: context is current trace sequence, x must be remaining trace sequence
  input_context <- keras::layer_input(shape = c(input_maxlen))
  target_sequence <- keras::layer_input(shape = c(target_maxlen))

  # encoder block
  context <- input_context %>%
    TokenAndPositionEmbedding_RemainingTrace()(maxlen = input_maxlen, vocab_size = vocab_size, d_model = d_model) %>%
    keras::layer_dropout(dropout_rate) %>%
    EncoderLayer()(d_model = d_model, num_heads = num_heads, dff = dff, dropout_rate = dropout_rate)

  # initiate decoder layer
  decoder <- DecoderLayer()(d_model = d_model, num_heads = num_heads, dff = dff, dropout_rate = dropout_rate)

  # decoder block
  x <- target_sequence %>%
    TokenAndPositionEmbedding_RemainingTrace()(maxlen = target_maxlen, vocab_size = vocab_size, d_model = d_model) %>%
    keras::layer_dropout(dropout_rate)

  x <- decoder(x = x, context = context) %>%
    keras::layer_dense(vocab_size, activation = "linear")

  # instantiate model
  keras::keras_model(list(input_context, target_sequence), x) -> model

  # add model and metrics to the list
  output <- list()
  class(output) <- c("ppred_model", class(output))
  output$model <- model

  # Same attributes as in `prep_remaining_trace2()`, but now stored as list-object components
  output$task <- "remaining_trace_s2s"
  output$vocabulary <- x_train %>% attr("vocabulary")
  output$vocab_size <- vocab_size
  output$max_case_length <- x_train %>% attr("max_case_length")
  output$input_maxlen <- input_maxlen
  output$target_maxlen <- target_maxlen

  return(output)
}

















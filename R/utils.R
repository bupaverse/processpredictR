


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
GlobalSelfAttention <- function() {
  GlobalSelfAttention(BaseAttention()) %py_class% {
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
CrossAttention <- function() {
  CrossAttention(BaseAttention()) %py_class% {
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
CausalSelfAttention <- function() {
  CausalSelfAttention(BaseAttention()) %py_class% {
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
FeedForward <- function() {
  FeedForward(keras$layers$Layer) %py_class% {
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



















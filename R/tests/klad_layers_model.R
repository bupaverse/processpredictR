# Transformer block -------------------------------------------------------
library(tensorflow)
library(keras)

TransformerBlock <- new_layer_class(
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

# TokenAndPositionEmbedding -----------------------------------------------
TokenAndPositionEmbedding <- new_layer_class(
  classname = "TokenAndPositionEmbedding",
  initialize = function(self, maxlen, vocab_size, embed_dim) {
    super$initialize()

    self$token_emb <- keras::layer_embedding(input_dim = vocab_size, output_dim = embed_dim) #layers.Embedding(input_dim=vocab_size, output_dim=embed_dim)
    self$pos_emb <- keras::layer_embedding(input_dim = maxlen, output_dim = embed_dim) #layers.Embedding(input_dim=maxlen, output_dim=embed_dim)

  },
  call = function(self, x) {
    maxlen <- tf$shape(x)[-1]  #tf.shape(x)[-1] NA, NULL, -1 is all the same
    positions <- tf$range(start=0, limit=maxlen, delta=1)
    positions <- self$pos_emb(positions)
    x <- self$token_emb(x)
    return(x + positions)
  }
)

# test definitie model
max_case_length <- 6 %>% as.integer()
embed_dim <- 36 %>% as.integer()
num_heads <- 4 %>% as.integer()
ff_dim <- 64 %>% as.integer()
rate <- 0.1

inputs <- layer_input(shape = c(6))
predictions <- inputs %>%
  TokenAndPositionEmbedding(maxlen = max_case_length, vocab_size = as.integer(9), embed_dim = embed_dim) %>%
  TransformerBlock(embed_dim = embed_dim, num_heads = num_heads, ff_dim = ff_dim) %>%
  keras::layer_global_average_pooling_1d()

keras::keras_model(inputs = inputs, outputs = predictions, name = "b") -> model

model


# model_ProcessTransformer <- new_model_class(
#   classname = "ProcessTransformer",
#   initialize = function(...) {
#     super$initialize()
#
#     self$TokenAndPositionEmbedding <- TokenAndPositionEmbedding(maxlen = max_case_length, vocab_size = as.integer(9), embed_dim = embed_dim)
#     self$TransformerBlock <- TransformerBlock(embed_dim = embed_dim, num_heads = num_heads, ff_dim = ff_dim)
#     self$glob_avg_pooling1D <- keras::layer_global_average_pooling_1d()
#
#   },
#   call = function(self, inputs) {
#     inputs %>%
#       self$TokenAndPositionEmbedding() %>%
#       self$TransformerBlock() %>%
#       self$glob_avg_pooling1D()
#   }
# )


# OUTCOME -----------------------------------------------------------------
# def get_outcome_transformer_model(max_case_length, num_features, vocab_size, output_dim, name, custom, embed_dim = 36, num_heads = 4, ff_dim = 64):
#   inputs = layers.Input(shape=(max_case_length,))
# if num_features > 0:
#   extra_inputs = layers.Input(shape=(num_features,))
# x = TokenAndPositionEmbedding(max_case_length, vocab_size, embed_dim)(inputs)
# x = TransformerBlock(embed_dim, num_heads, ff_dim)(x)
# x = layers.GlobalAveragePooling1D()(x)
# if num_features > 0:
#   x_extra = layers.Dense(32, activation="relu")(extra_inputs)
# x = layers.Concatenate()([x, extra_inputs])
# if custom == "default":
#   x = layers.Dropout(0.1)(x)
# x = layers.Dense(64*(num_features+1), activation="relu")(x)
# x = layers.Dropout(0.1)(x)
# x = layers.Dense(output_dim, activation="linear")(x)
# if num_features > 0:
#   transformer = tf.keras.Model(inputs=[inputs, extra_inputs], outputs=x, name = name) #name = "outcome_OR_nextActivity_transformer")
# else:
#   transformer = tf.keras.Model(inputs=inputs, outputs=x, name = name)
# return transformer



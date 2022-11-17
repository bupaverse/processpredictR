import tensorflow as tf
from tensorflow.keras import layers
from tensorflow.keras import activations


class TransformerBlock(layers.Layer):
    def __init__(self, embed_dim, num_heads, ff_dim, rate=0.1):
        super(TransformerBlock, self).__init__()
        self.att = layers.MultiHeadAttention(num_heads=num_heads, key_dim=embed_dim)
        self.ffn = tf.keras.Sequential(
            [layers.Dense(ff_dim, activation="relu"), layers.Dense(embed_dim),]
        )
        self.layernorm_a = layers.LayerNormalization(epsilon=1e-6)
        self.layernorm_b = layers.LayerNormalization(epsilon=1e-6)
        self.dropout_a = layers.Dropout(rate)
        self.dropout_b = layers.Dropout(rate)

    def call(self, inputs, training):
        attn_output = self.att(inputs, inputs)
        attn_output = self.dropout_a(attn_output, training=training)
        out_a = self.layernorm_a(inputs + attn_output)
        ffn_output = self.ffn(out_a)
        ffn_output = self.dropout_b(ffn_output, training=training)
        return self.layernorm_b(out_a + ffn_output)


class TokenAndPositionEmbedding(layers.Layer):
    def __init__(self, maxlen, vocab_size, embed_dim):
        super(TokenAndPositionEmbedding, self).__init__()
        self.token_emb = layers.Embedding(input_dim=vocab_size, output_dim=embed_dim)
        self.pos_emb = layers.Embedding(input_dim=maxlen, output_dim=embed_dim)

    def call(self, x):
        maxlen = tf.shape(x)[-1]
        positions = tf.range(start=0, limit=maxlen, delta=1)
        positions = self.pos_emb(positions)
        x = self.token_emb(x)
        return x + positions



def get_outcome_transformer_model(max_case_length, num_features, vocab_size, output_dim, name, embed_dim = 36, num_heads = 4, ff_dim = 64):
  inputs = layers.Input(shape=(max_case_length,))
  if num_features > 0: 
    extra_inputs = layers.Input(shape=(num_features,))
  x = TokenAndPositionEmbedding(max_case_length, vocab_size, embed_dim)(inputs)
  x = TransformerBlock(embed_dim, num_heads, ff_dim)(x)
  x = layers.GlobalAveragePooling1D()(x)
  if num_features > 0: 
    x_extra = layers.Dense(32, activation="relu")(extra_inputs)
    x = layers.Concatenate()([x, extra_inputs])
  x = layers.Dropout(0.1)(x)
  x = layers.Dense(64, activation="relu")(x)
  x = layers.Dropout(0.1)(x)
  outputs = layers.Dense(output_dim, activation="linear")(x)
  transformer = tf.keras.Model(inputs=inputs, outputs=outputs, name = name) #name = "outcome_OR_nextActivity_transformer")
  
  return transformer



def get_next_time_model(max_case_length, num_features, vocab_size, output_dim, name, embed_dim = 36, num_heads = 4, ff_dim = 64):
  inputs = layers.Input(shape=(max_case_length,))
  # Three time-based features
  time_inputs = layers.Input(shape=(num_features,)) 
  x = TokenAndPositionEmbedding(max_case_length, vocab_size, embed_dim)(inputs)
  x = TransformerBlock(embed_dim, num_heads, ff_dim)(x)
  x = layers.GlobalAveragePooling1D()(x)
  x_t = layers.Dense(32, activation="relu")(time_inputs)
  x = layers.Concatenate()([x, x_t])
  x = layers.Dropout(0.1)(x)
  x = layers.Dense(128, activation="relu")(x)
  x = layers.Dropout(0.1)(x)
  outputs = layers.Dense(output_dim, activation="linear")(x)
  transformer = tf.keras.Model(inputs=[inputs, time_inputs], outputs=outputs, name = name)
  
  return transformer


def get_remaining_time_model(max_case_length, num_features, vocab_size, output_dim, name, embed_dim = 36, num_heads = 4, ff_dim = 64):
  inputs = layers.Input(shape=(max_case_length,))
  # Three time-based features
  time_inputs = layers.Input(shape=(num_features,)) 
  x = TokenAndPositionEmbedding(max_case_length, vocab_size, embed_dim)(inputs)
  x = TransformerBlock(embed_dim, num_heads, ff_dim)(x)
  x = layers.GlobalAveragePooling1D()(x)
  x_t = layers.Dense(32, activation="relu")(time_inputs)
  x = layers.Concatenate()([x, x_t])
  x = layers.Dropout(0.1)(x)
  x = layers.Dense(128, activation="relu")(x)
  x = layers.Dropout(0.1)(x)
  outputs = layers.Dense(output_dim, activation="linear")(x)
  transformer = tf.keras.Model(inputs=[inputs, time_inputs], outputs=outputs,
  name = name)
  
  return transformer



def get_remaining_trace_model(max_case_length, num_features, vocab_size, output_dim, name, embed_dim = 36, num_heads = 4, ff_dim = 64):
  inputs = layers.Input(shape=(max_case_length,))
  if num_features > 0: 
    extra_inputs = layers.Input(shape=(num_features,))
  x = TokenAndPositionEmbedding(max_case_length, vocab_size, embed_dim)(inputs)
  x = TransformerBlock(embed_dim, num_heads, ff_dim)(x)
  x = layers.GlobalAveragePooling1D()(x)
  if num_features > 0: 
    x_extra = layers.Dense(32, activation="relu")(extra_inputs)
    x = layers.Concatenate()([x, x_extra])
  x = layers.Dropout(0.1)(x)
  x = layers.Dense(64, activation="relu")(x)
  x = layers.Dropout(0.1)(x)
  outputs = layers.Dense(output_dim, activation="linear")(x)
  transformer = tf.keras.Model(inputs=inputs, outputs=outputs, name = name)
  
  return transformer


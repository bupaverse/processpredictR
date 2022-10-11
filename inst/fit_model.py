import keras as keras
import tensorflow as tf

callback_list = [
        tf.keras.callbacks.TensorBoard("tensorboard", histogram_freq=1, write_graph=True),
        tf.keras.callbacks.CSVLogger("logs", append = True),
        tf.keras.callbacks.EarlyStopping(monitor='val_loss', patience=5, verbose=1)
        ]


def compile_model(transformer_model, learning_rate = 0.001):
  transformer_model.compile(
    optimizer=tf.keras.optimizers.Adam(learning_rate),
    loss=tf.keras.losses.SparseCategoricalCrossentropy(from_logits=True),
    metrics=[tf.keras.metrics.SparseCategoricalAccuracy()])
#     
# # def compile_model(transformer_model, learning_rate = 0.001):
# #   transformer_model.compile(
# #     optimizer=tf.keras.optimizers.Adam(learning_rate),
# #     loss=tf.keras.losses.BinaryCrossentropy(from_logits=True),
# #     metrics=[tf.keras.metrics.BinaryAccuracy()])

def fit_model(transformer_model, train_token_x, train_token_y, num_epochs, batch_size, file):
  transformer_model.fit(train_token_x, train_token_y, 
  epochs=num_epochs,
  batch_size=batch_size, 
  shuffle=True,
  verbose=1,
  validation_split=0.2,
  callbacks=callback_list)
  
  transformer_model.save(file)

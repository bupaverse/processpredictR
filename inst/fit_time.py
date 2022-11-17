callback_list = [
  tf.keras.callbacks.TensorBoard("tensorboard", histogram_freq=1, write_graph=True),
  tf.keras.callbacks.CSVLogger("logs", append = True)#,
  #tf.keras.callbacks.EarlyStopping(monitor='val_loss', patience=5, verbose=1)
]

def compile_model_py(transformer_model, learning_rate):
  transformer_model.compile(
    optimizer=tf.keras.optimizers.Adam(learning_rate),
    loss=tf.keras.losses.LogCosh())





def fit_model_py(transformer_model, train_token_x, train_time_x, train_token_y, num_epochs, batch_size, file):
  transformer_model.fit(
    [train_token_x, train_time_x], train_token_y, 
    epochs=num_epochs, batch_size=batch_size, 
    verbose=2, callbacks= callback_list)
    
  transformer_model.save(file)

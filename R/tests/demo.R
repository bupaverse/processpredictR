# outcome-default ---------------------------------------------------------
library(processpredictR)
library(eventdataR)

# preprocess log
df <- prepare_examples(log = patients, task = "outcome")
df %>% View

# split df into train- and test
split <- split_train_test(processed_df = df, split = 0.7, trace_length_bins = 5)
split %>% View
df_train <- split$train_df
df_test <- split$test_df

# create default model
model <- create_model(processed_df = df, custom_model_py = "default")
model

# compile model
compile_model(transformer_model = model, learning_rate = 0.001)

# fit model
model %>% fit_model(train_data = df_train, num_epochs = 2, batch_size = 10, file = "demo")

# make predictions
y_pred <- model %>% predict_model(test_data = df_test)
y_pred

# metrics
library(Metrics)
df_test %>% tokenize() -> tokens
tokens %>% print.default()
detokenize(y_pred, tokens$token_y, processed_df = df) -> result
result
Metrics::accuracy(actual = result$y_actual, predicted = result$token)

keras::tensorboard(log_dir = "tensorboard/")


# extra features ----------------------------------------------------------
# standard features
df_time <- prepare_examples(patients, task = "next_time")
df_time %>% attr("features")

# extra feature
df_time <- prepare_examples(patients, task = "next_time", features = "employee")
df_time %>% attr("features")

model_time <- create_model(processed_df = df_time)
model_time

compile_model(model_time, learning_rate = 0.001)
fit_model(model_time, df_time, num_epochs = 1, batch_size = 15, file = "demo")

df_time %>% tokenize() %>% print.default()



# custom model ------------------------------------------------------------
df_activity <- prepare_examples(patients, task = "next_activity")
model_activity <- create_model(processed_df = df_activity, custom_model_py = "custom")
model_activity

model_activity$output %>%
  keras::layer_dropout(rate = 0.1) %>%
  keras::layer_dense(units = 64, activation = 'relu') %>%
  keras::layer_dropout(rate = 0.1) %>%
  keras::layer_dense(units = num_outputs(df_activity), activation = 'linear') -> new_output

custom_model <- keras::keras_model(inputs = model_activity$input, outputs = new_output, name = "next_activity")
custom_model

# mapping of class and attributes to a new custom model
custom_model <- map_attributes(model = custom_model, processed_df = df_activity)
custom_model



# generic fit -------------------------------------------------------------
df_activity %>%
  tokenize() -> tokens

train_token_x <- tokens$token_x %>% keras::pad_sequences(maxlen = max_case_length(df_activity), value = 0)
train_token_y <- tokens$token_y

# compile
compile_model(custom_model, 0.001)
# generic fit
custom_model %>%
  keras::fit(train_token_x, train_token_y, batch_size = 10, verbose = 1, epochs = 5, validation_split=0.2)




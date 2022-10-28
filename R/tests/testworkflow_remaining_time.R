# testworkflow_remaining_time ---------------------------------------------

#preprocess dataset
df <- prepare_examples(eventdataR::patients, task = "remaining_time")
df

#split dataset into train- and test dataset
df_train_test <- split_train_test(df, ratio = 0.7)
df_train <- df_train_test$train_df
df_test <- df_train_test$test_df

#tokenize train dataset
tokens_train <- tokenize(df_train, vocabulary = create_vocabulary(df))
tokens_train

#define transformer model
model <- transformer_model(df)
model

#compile transformer model
transformer_compile(transformer_model = model, learning_rate = 0.001)

#fit transformer model
transformer_fit(transformer_model = model, tokens_train = tokens_train,
                maxlen = max_case_length(df), num_epochs = 10, batch_size = 12, file = "example_model_remaining_time")

#tokenize test dataset
tokens_test <- tokenize(df_test, vocabulary = create_vocabulary(df))

#predict on test data
results <- transformer_predict(transformer_model = model, tokens_test = tokens_test, maxlen = max_case_length(df), predict_type = "metrics")
results

#get the predicted values y_pred
y_pred_scaled <- transformer_predict(transformer_model = model, tokens_test = tokens_test, maxlen = max_case_length(df), predict_type = "y_pred")
y_pred_scaled %>% as.vector()

scale(df_test$remaining_time) -> standardScaled
standardScaled

y_pred <- (y_pred_scaled %>% as.vector() * attr(standardScaled, 'scaled:scale') + attr(standardScaled, 'scaled:center'))
y_pred %>% summary()


MAPE <- mean(abs((tokens_test$token_y - y_pred)/tokens_test$token_y)) * 100
MAPE
r2_score <- cor(tokens_test$token_y,y_pred)^2
r2_score

Metrics::mae(tokens_test$token_y, y_pred)
Metrics::rmse(tokens_test$token_y, y_pred)


# tensorboard
keras::tensorboard(log_dir = "tensorboard/")




















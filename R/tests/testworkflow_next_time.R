# testworkflow_next_time --------------------------------------------------

#preprocess dataset
df <- prepare_examples(eventdataR::patients, task = "next_time")
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
                maxlen = max_case_length(df), num_epochs = 5, batch_size = 12, file = "example_model_next_time")


#tokenize test dataset
tokens_test <- tokenize(df_test, vocabulary = create_vocabulary(df))

#predict on test data
results <- transformer_predict(transformer_model = model, tokens_test = tokens_test, maxlen = max_case_length(df), predict_type = "metrics")
results

#get the predicted values y_pred and calculate metrics
y_pred <- transformer_predict(transformer_model = model, tokens_test = tokens_test, maxlen = max_case_length(df), predict_type = "y_pred")
y_pred %>% as.vector()

scale(df_test$next_time) -> standardScaled
standardScaled

(y_pred %>% as.vector() * attr(standardScaled, 'scaled:scale') + attr(standardScaled, 'scaled:center')) %>% summary()

r2_score <- cor(tokens_test$token_y,y_pred)^2
r2_score # how good is the model at predicting

Metrics::mae(tokens_test$token_y, y_pred)
Metrics::rmse(tokens_test$token_y, y_pred) # average deviation between predicted and actual value

# tensorboard
keras::tensorboard(log_dir = "tensorboard/")












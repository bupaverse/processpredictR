# testworkflow_next_activity ----------------------------------------------

#preprocess dataset
df <- create_prefix_df(traffic_fines, prediction = "next_activity")
df

#split dataset into train- and test dataset
split_train_test_df(df, ratio = 0.7)
df_train <- split_train_test_df(df, ratio = 0.7)$train_df
df_test <- split_train_test_df(df, ratio = 0.7)$test_df

#tokenize train dataset
tokens_train <- tokenize(df_train)
tokens_train

#define transformer model
model <- transformer_model(df)
model

#compile transformer model
transformer_compile(transformer_model = model, learning_rate = 0.001)

#fit transformer model
transformer_fit(transformer_model = model, tokens_train = tokens_train,
                maxlen = max_case_length(df), num_epochs = 15, batch_size = 12, file = "example_model_next_activity")


#tokenize test dataset
tokens_test <- tokenize(df_test)

#predict on test data
results <- transformer_predict(transformer_model = model, tokens_test = tokens_test, maxlen = max_case_length(df))
results













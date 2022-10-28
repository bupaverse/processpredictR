# testworkflow_outcome ----------------------------------------------------

#preprocess dataset
acts <- patients %>% end_activities("activity")
acts <- unique(acts$activity) %>% as.character()

df <- prepare_examples(patients, task = "outcome", outcome_label1 = "Check-out",
                       outcome_label2 = acts[-1])
df


## split processed dataframe into train- and test dataframes
df_train_test <- split_train_test(df, ratio = 0.7)
df_train <- df_train_test$train_df
df_test <- df_train_test$test_df

#tokenize train dataset
tokens_train <- tokenize(df_train, vocabulary = create_vocabulary(df))
tokens_train

model <- transformer_model(df)
model

#compile transformer model
transformer_compile(transformer_model = model, learning_rate = 0.001)

#fit transformer model
transformer_fit(transformer_model = model, tokens_train = tokens_train,
                maxlen = max_case_length(df), num_epochs = 5, batch_size = 12, file = "example_model_outcome")

#tokenize test dataset
tokens_test <- tokenize(df_test, vocabulary = create_vocabulary(df))

#predict on test data
results <- transformer_predict(transformer_model = model, tokens_test = tokens_test, maxlen = max_case_length(df), predict_type = "metrics")
results

#visualize with tensorboard
keras::tensorboard(log_dir = "tensorboard/")


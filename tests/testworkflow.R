#TEST WORKFLOW
## assign outcome labels according to end activities from the event log patients
patients %>% end_activities("activity")
processedf <- preprocess_log(patients, outcome1 = "Check-out", outcome2 = c("Discuss Results", "Triage and Assessment",
                                                                                   "X-Ray", "Blood test"))
processedf


## split processed dataframe into train- and test dataframes
train_test_df <- split_train_test_df(processedf)
train_test_df

processedf_train <- train_test_df$train_df
processedf_test <- train_test_df$test_df


## tokenize processedf_train into feature tokens (token_x) and target tokens (token_y)
tokens_train <- tokenize(processedf_train, activities = activities_list(patients))
# tokens_train_x <- tokens_train$token_x
# tokens_train_y <- tokens_train$token_y
tokens_train


## Define transformer model
maxlen <- max_case_length(patients)
vocab <- vocab_size(patients, outcome_names = c("outcome1", "outcome2"))
test_model <- transformer_model(max_case_length = maxlen, vocab_size = vocab, num_output = 2)
test_model

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




vocab <- vocab_size(patients, outcome_names = c("outcome1", "outcome2")) # MUST EQUAL 9 or 11? + outcomes or without. At this point =11
vocab
# activity_names <- patients %>% activity_labels() %>% as.character()
# activity_names
# activity_names <- c("PAD", "UNK") %>%
#   append(activity_names)
# length(unique(append(activity_names, c("outcome1", "outcome2"))))  %>% as.integer()



test_model <- transformer_model(max_case_length = maxlen, vocab_size = vocab, num_output = 2)
test_model


## Compile and fit model
compile_test(test_model, learning_rate = 0.001)

fit_test(transformer_model = test_model, tokens_train = tokens_train, num_epochs = as.integer(10), batch_size = as.integer(12), file = "test_model")






# TEST traffic_fines ------------------------------------------------------
## assign outcome labels according to end activities from the event log patients
acts <- traffic_fines %>% end_activities("activity") # look on distributions of end activities per case
acts
acts <- unique(acts$activity) %>% as.character()
processedf <- preprocess_log(traffic_fines, outcome1 = "Payment", outcome2 = acts[-1])
processedf


## split processed dataframe into train- and test dataframes
train_test_df <- split_train_test_df(processedf)
train_test_df

processedf_train <- train_test_df$train_df
processedf_test <- train_test_df$test_df



## tokenize processedf_train into feature tokens (token_x) and target tokens (token_y)
tokens_train <- tokenize(processedf_train, activities = activities_list(traffic_fines))
# tokens_train_x <- tokens_train$token_x
# tokens_train_y <- tokens_train$token_y
tokens_train


## Define transformer model
maxlen <- max_case_length(traffic_fines)


vocab <- vocab_size(traffic_fines, outcome_names = c("outcome1", "outcome2")) # MUST EQUAL 9 or 11? + outcomes or without. At this point =11
vocab
# activity_names <- patients %>% activity_labels() %>% as.character()
# activity_names
# activity_names <- c("PAD", "UNK") %>%
#   append(activity_names)
# length(unique(append(activity_names, c("outcome1", "outcome2"))))  %>% as.integer()



test_model <- transformer_model(max_case_length = maxlen, vocab_size = vocab, num_output = 2)
test_model


## Compile and fit model
compile_test(test_model, learning_rate = 0.001)

fit_test(transformer_model = test_model, tokens_train = tokens_train, maxlen = maxlen, num_epochs = as.integer(10), batch_size = as.integer(12), file = "test_model")






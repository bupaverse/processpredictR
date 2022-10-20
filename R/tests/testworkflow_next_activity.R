# testworkflow_next_activity ----------------------------------------------

#preprocess dataset
df <- create_prefix_df(eventdataR::patients, prediction = "next_activity")
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
                maxlen = max_case_length(df), num_epochs = 5, batch_size = 12, file = "example_model_next_activity")


#tokenize test dataset
tokens_test <- tokenize(df_test)

#predict on test data
results <- transformer_predict(transformer_model = model, tokens_test = tokens_test, maxlen = max_case_length(df), predict_type = "y_pred")
results

#visualize with tensorboard
keras::tensorboard(log_dir = "tensorboard/")





#example
# given a case with a following trace of activities up until now, what is the most likely next activity?

create_vocabulary(df, type = "outcomes") -> voc_outs

as.data.frame(results) %>%
  left_join(voc_outs, by = c("results" = "key_id")) %>% mutate(actual = tokens_test$token_y) %>%
  ggplot(aes(results, actual)) +
  geom_point() + geom_jitter()

results
voc_outs

ex_trace <- c("Registration", "Triage and Assessment", "MRI SCAN")


create_vocabulary(df, type = "activities") -> voc_acts



# #Probabilities in R
# model %>% predict(tokens_test$token_x) %>% k_argmax()
# tokens_test$token_x %>% length()
# tokens_test$token_y %>% length()
#
# model %>% predict(purrr::map(tokens_test, ~ append(testvec, .x))) %>% k_argmax()
#
# testvec <- c()
#
# testvec <- purrr::map(tokens_test$token_x, ~ append(testvec, .x))
#
# testvec



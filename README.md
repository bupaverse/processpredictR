# processpredictR
Predictions on processes from event log using Transformer model.  
Prediction tasks:
- [x] outcome
- [x] next activity
- [x] next time <sup>requires an event- or activity log to have both start- and end timestamps</sup>
- [ ] remaining time <sup>requires an event- or activity log to have both start- and end timestamps</sup>
- [ ] remaining trace  

## Example next activity prediction
### Installation
```r
install.packages("https://github.com/bupaverse/processpredictR.git")
library(processpredictR)
```

### preprocess dataset
```r
df <- create_prefix_df(traffic_fines, prediction = "next_activity")
df
```

### split dataset into train- and test dataset
```r
split_train_test_df(df, ratio = 0.7)
df_train <- split_train_test_df(df, ratio = 0.7)$train_df
df_test <- split_train_test_df(df, ratio = 0.7)$test_df
```

### tokenize train dataset
```r
tokens_train <- tokenize(df_train)
tokens_train
```

### define transformer model
```r
model <- transformer_model(df)
model
```

### compile transformer model
```r
transformer_compile(transformer_model = model, learning_rate = 0.001)
```

### fit transformer model
```r
transformer_fit(transformer_model = model, tokens_train = tokens_train,
                maxlen = max_case_length(df), num_epochs = 15, batch_size = 12, file = "example_model_next_activity")
```

### tokenize test dataset
```r
tokens_test <- tokenize(df_test)
```

### predict on test data
```r
results <- transformer_predict(transformer_model = model, tokens_test = tokens_test, maxlen = max_case_length(df))
results
```

### visualize with tensorboard
```r
tensorboard(log_dir = "tensorboard/")
```











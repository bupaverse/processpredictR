
<!-- README.md is generated from README.Rmd. Please edit that file -->

# processpredictR

<!-- badges: start -->
<!-- badges: end -->

The goal of processpredictR is to perform prediction tasks on processes
using event logs and Transformer models.

## Installation

You can install the development version of processpredictR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bupaverse/processpredictR")
```

## Prediction tasks:

-   [x] outcome
-   [x] next activity
-   [x] next time
-   [x] remaining time
-   [x] remaining trace

## Examples

<details>
<summary>
Next activity prediction
</summary>
<p>

### Installation

``` r
install.packages("https://github.com/bupaverse/processpredictR.git")
```

``` r
library(processpredictR)
library(eventdataR)
```

### preprocess dataset

``` r
df <- prepare_examples(patients, task = "next_time")
df
```

### split dataset into train- and test dataset

``` r
set.seed(123)
split <- split_train_test(df, split = 0.7, trace_length_bins = 5)
df_train <- split$train_df
df_test <- split$test_df
```

### tokenize train dataset

``` r
tokens_train <- tokenize(df_train, vocabulary = create_vocabulary(df))
tokens_train
```

### define transformer model

``` r
model <- transformer_model(df)
model
```

### compile transformer model

``` r
transformer_compile(transformer_model = model, learning_rate = 0.001)
```

### fit transformer model

``` r
transformer_fit(transformer_model = model, tokens_train = tokens_train,
                maxlen = max_case_length(df), num_epochs = 15, batch_size = 12, file = "example_model_next_activity")
```

### tokenize test dataset

``` r
tokens_test <- tokenize(df_test, vocabulary = create_vocabulary(df))
```

### predict on test data

``` r
results <- transformer_predict(transformer_model = model, tokens_test = tokens_test, maxlen = max_case_length(df))
results
```

### visualize with tensorboard

``` r
tensorboard(log_dir = "tensorboard/")
```

</p>
</details>
<details>
<summary>
Next time prediction
</summary>
<p>

### Installation

``` r
install.packages("https://github.com/bupaverse/processpredictR.git")
library(processpredictR)
```

### preprocess dataset

``` r
df <- create_prefix_df(eventdataR::patients, prediction = "next_time")
df
```

### split dataset into train- and test dataset

``` r
set.seed(123)
split <- split_train_test(df, split = 0.7, trace_length_bins = 5)
df_train <- split$train_df
df_test <- split$test_df
```

### tokenize train dataset

``` r
tokens_train <- tokenize(df_train, vocabulary = create_vocabulary(df))
tokens_train
```

### define transformer model

``` r
model <- transformer_model(df)
model
```

### compile transformer model

``` r
transformer_compile(transformer_model = model, learning_rate = 0.001)
```

### fit transformer model

``` r
transformer_fit(transformer_model = model, tokens_train = tokens_train,
                maxlen = max_case_length(df), num_epochs = 10, batch_size = 12, file = "example_model_next_time")
```

### tokenize test dataset

``` r
tokens_test <- tokenize(df_test, vocabulary = create_vocabulary(df))
```

### predict on test data

``` r
results <- transformer_predict(transformer_model = model, tokens_test = tokens_test, maxlen = max_case_length(df), predict_type = "metrics")
results
```

### get the predicted values y_pred and calculate metrics

``` r
y_pred <- transformer_predict(transformer_model = model, tokens_test = tokens_test, maxlen = max_case_length(df), predict_type = "y_pred")
y_pred %>% as.vector()
```

``` r
scale(df_test$next_time) -> standardScaled
standardScaled

(y_pred %>% as.vector() * attr(standardScaled, 'scaled:scale') + attr(standardScaled, 'scaled:center')) %>% summary()

MAPE <- mean(abs((tokens_test$token_y-y_pred)/tokens_test$token_y))*100
MAPE
r2_score <- cor(tokens_test$token_y,y_pred)^2
r2_score

Metrics::mae(tokens_test$token_y, y_pred)
Metrics::rmse(tokens_test$token_y, y_pred)
```

### tensorboard

``` r
keras::tensorboard(log_dir = "tensorboard/")
```

</p>
</details>

## Attribution

This repository is based on the ProcessTransformer library by Buksh et
al. Citation: Zaharah A. Bukhsh, Aaqib Saeed, & Remco M. Dijkman.
(2021). “ProcessTransformer: Predictive Business Process Monitoring with
Transformer Network”. arXiv preprint arXiv:2104.00721

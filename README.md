
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

## Examples

``` r
library(processpredictR)
library(eventdataR)
```

<details>
<summary>
Outcome prediction
</summary>
<p>

``` r
library(processpredictR)
library(eventdataR)
```

### preprocess dataset

``` r
df <- prepare_examples(patients, task = "outcome")
df
```

### split dataset into train- and test dataset

``` r
set.seed(123)
split <- split_train_test(df, split = 0.7, trace_length_bins = 5)
split
split$train_df -> df_train
split$test_df -> df_test
```

### define transformer model

``` r
model <- create_model(df)
model
```

### compile transformer model

``` r
compile_model(transformer_model = model, learning_rate = 0.001)
```

### fit transformer model

``` r
fit_model(model, train_data = df_train, num_epochs = 5, batch_size = 10, file = "outcome")
```

### predict on test data

``` r
result <- predict_model(transformer_model = model, test_data = df_test)
result
```

### visualize with tensorboard

``` r
tensorboard(log_dir = "tensorboard/")
```

</p>
</details>
<details>
<summary>
Next activity prediction
</summary>
<p>

### preprocess dataset

``` r
df <- prepare_examples(patients, task = "next_activity")
df
```

### split dataset into train- and test dataset

``` r
set.seed(123)
split <- split_train_test(df, split = 0.7, trace_length_bins = 5)
split
split$train_df -> df_train
split$test_df -> df_test
```

### define transformer model

``` r
model <- create_model(df)
model
```

### compile transformer model

``` r
compile_model(transformer_model = model, learning_rate = 0.001)
```

### fit transformer model

``` r
fit_model(model, train_data = df_train, num_epochs = 5, batch_size = 10, file = "next_activity")
```

### predict on test data

``` r
result <- predict_model(transformer_model = model, test_data = df_test)
result
```

</p>
</details>
<details>
<summary>
Next time prediction
</summary>
<p>

### preprocess dataset

``` r
df <- prepare_examples(patients, task = "next_time")
df
```

### split dataset into train- and test dataset

``` r
set.seed(123)
split <- split_train_test(df, split = 0.7, trace_length_bins = 5)
split
split$train_df -> df_train
split$test_df -> df_test
```

### define transformer model

``` r
model <- create_model(df)
model
```

### compile transformer model

``` r
compile_model(transformer_model = model, learning_rate = 0.001)
```

### fit transformer model

``` r
fit_model(model, train_data = df_train, num_epochs = 5, batch_size = 10, file = "next_time")
```

### predict on test data

``` r
result <- predict_model(transformer_model = model, test_data = df_test)
result
```

### calculate metrics (todo: create function)

</p>
</details>
<details>
<summary>
Remaining time prediction
</summary>
<p>

### preprocess dataset

``` r
df <- prepare_examples(patients, task = "remaining_time")
df
```

### split dataset into train- and test dataset

``` r
set.seed(123)
split <- split_train_test(df, split = 0.7, trace_length_bins = 5)
split
split$train_df -> df_train
split$test_df -> df_test
```

### define transformer model

``` r
model <- create_model(df)
model
```

### compile transformer model

``` r
compile_model(transformer_model = model, learning_rate = 0.001)
```

### fit transformer model

``` r
fit_model(model, train_data = df_train, num_epochs = 5, batch_size = 10, file = "remaining_time")
```

### predict on test data

``` r
result <- predict_model(transformer_model = model, test_data = df_test)
result
```

### calculate metrics (todo: create function)

</p>
</details>
<details>
<summary>
Remaining_trace prediction
</summary>
<p>

### preprocess dataset

``` r
df <- prepare_examples(patients, task = "remaining_trace")
df
```

### split dataset into train- and test dataset

``` r
set.seed(123)
split <- split_train_test(df, split = 0.7, trace_length_bins = 5)
split
split$train_df -> df_train
split$test_df -> df_test
```

### define transformer model

``` r
model <- create_model(df)
model
```

### compile transformer model

``` r
compile_model(transformer_model = model, learning_rate = 0.001)
```

### fit transformer model

``` r
fit_model(model, train_data = df_train, num_epochs = 5, batch_size = 10, file = "remaining_trace")
```

### predict on test data

``` r
result <- predict_model(transformer_model = model, test_data = df_test)
result
```

</p>
</details>

## Attribution

This repository is based on the ProcessTransformer library by Buksh et
al. Citation: Zaharah A. Bukhsh, Aaqib Saeed, & Remco M. Dijkman.
(2021). “ProcessTransformer: Predictive Business Process Monitoring with
Transformer Network”. arXiv preprint arXiv:2104.00721

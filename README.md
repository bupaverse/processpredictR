
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

### preprocess dataset

``` r
df <- prepare_examples_dt(patients, task = "outcome")
df
```

### split dataset into train- and test dataset

``` r
set.seed(123)
split <- split_train_test(df, split = 0.7)
split
split$train_df -> train
split$test_df -> test
```

### define transformer model

``` r
model <- create_model(df)
model
```

### compile transformer model

``` r
compile(model)
```

### fit transformer model

``` r
fit(model, train, num_epochs = 5, batch_size = 10)
```

### predict on test data

``` r
result <- predict(model, test)
result
```

### evaluate

``` r
result <- evaluate(model, test)
result
```

### visualize with tensorboard

``` r
tensorboard(log_dir = "tensorboard/")
```

## Attribution

This repository is based on the ProcessTransformer library by Buksh et
al. Citation: Zaharah A. Bukhsh, Aaqib Saeed, & Remco M. Dijkman.
(2021). “ProcessTransformer: Predictive Business Process Monitoring with
Transformer Network”. arXiv preprint arXiv:2104.00721

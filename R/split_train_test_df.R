#' Splits processed dataframe into train- and/or test dataframes
#'
#' (WIP)
#'
#' @param processed_df A preprocessed dataframe from preprocess_log function
#' @param ratio A train-test split ratio
#' @param type_df A type of dataframe to return c("train", "test")
#' @return if type_df is NULL returns a list of train and test dataframes, otherwise either train or test dataframe by specifying type_df
#'
#' @export
split_train_test_df <- function(processed_df, ratio = 0.7, type_df = NULL) { # OR n_distinct(log[case_id(log)])
  # maybe add shuffle() later

  unique_cases <- unique(processed_df$ith_case)
  split <- n_distinct(unique_cases) * ratio # choose a specified split

  unique_cases_train <- unique_cases[1:split] # train dataset
  unique_cases_train <- data.frame(ith_case = unique_cases_train)

  unique_cases_test <- unique_cases[(split+1):n_distinct(unique_cases)]
  unique_cases_test <- data.frame(ith_case = unique_cases_test)

  train_df <- inner_join(processed_df, unique_cases_train, by = "ith_case")
  test_df <- inner_join(processed_df, unique_cases_test, by = "ith_case")


  if (type_df %>% is.null()) {
    list(train_df = train_df, test_df = test_df)
  }

  else if (type_df == "train") {
    train_df
  }

  else if (type_df == "test") {
    test_df
  }

}

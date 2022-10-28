#' Splits processed dataframe into train- and/or test dataframes
#'
#' (WIP)
#'
#' @param processed_df A preprocessed dataframe from preprocess_log function
#' @param ratio A train-test split ratio
#' @param trace_length_bins Number of trace length bins to use for stratification
#' @return  a list of train and test dataframes
#'
#' @export
split_train_test <- function(processed_df, ratio = 0.7, trace_length_bins = NULL) { # OR n_distinct(log[case_id(log)])

  if (is.null(trace_length_bins)) {
    set.seed(123)
    unique_cases <- unique(processed_df$ith_case)
    sample_size <- n_distinct(unique_cases) * 0.7 # choose a specified sample_size
    train_ind <- sample(unique(processed_df$ith_case), size = sample_size)

    # id's train
    unique_cases_train <- unique_cases[train_ind] # train dataset
    unique_cases_train <- data.frame(ith_case = unique_cases_train)

    # id's test
    unique_cases_test <- unique_cases[-train_ind]
    unique_cases_test <- data.frame(ith_case = unique_cases_test)

    # join with processed_df
    train_df <- inner_join(processed_df, unique_cases_train, by = "ith_case")
    test_df <- inner_join(processed_df, unique_cases_test, by = "ith_case")

    list(train_df = train_df, test_df = test_df)

  }

  # x %>% mutate(trace_length = lengths(traces)) %>% mutate(trace_bucket = ntile(trace_length, max(trace_length)))
  else {

    processed_df %>%
      mutate(case_tile = ntile(ith_case, trace_length_bins))

  }


#
# unique_cases_train <- unique_cases[1:sample_size] # train dataset
# unique_cases_train <- data.frame(ith_case = unique_cases_train)
#
# unique_cases_test <- unique_cases[(sample_size+1):n_distinct(unique_cases)]
# unique_cases_test <- data.frame(ith_case = unique_cases_test)
#
# train_df <- inner_join(processed_df, unique_cases_train, by = "ith_case")
# test_df <- inner_join(processed_df, unique_cases_test, by = "ith_case")
#
#   list(train_df = train_df, test_df = test_df)

}

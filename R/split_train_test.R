#' Splits the preprocessed [`data.frame`].
#'
#' @description Returns train- and test dataframes as a list.
#'
#' @param processed_df A preprocessed object of type [`log`] or [`data.frame`] from prepare_examples function.
#' @param split [`numeric`] (default 0.7): A train-test split ratio.
#' @param trace_length_bins [`numeric`] (default [`NULL`]): A number of trace length bins to use for stratification.
#' If [`NULL`], does not stratify for similar trace length in both train- and test dataframes.
#'
#' @export
split_train_test <- function(processed_df, split = 0.7, trace_length_bins = 5) { # OR n_distinct(log[case_id(log)])

  if (is.null(trace_length_bins)) {
    unique_cases <- unique(processed_df$ith_case)
    sample_size <- n_distinct(unique_cases) * split # choose a specified sample_size
    train_ind <- sample(unique(processed_df$ith_case), size = sample_size)

    # id's train
    unique_cases_train <- unique_cases[train_ind] # train dataset
    train_df <-  processed_df %>% filter(ith_case %in% unique_cases_train)

    # id's test
    test_df <- processed_df %>% filter(!(ith_case %in% unique_cases_train))

    list(train_df = train_df, test_df = test_df)

  }

  # x %>% mutate(trace_length = lengths(traces)) %>% mutate(trace_bucket = ntile(trace_length, max(trace_length)))
  else {

    processed_df %>%
      #make list of case id + trace length
      group_by(ith_case) %>%
      summarize(trace_length = max(k) + 1) %>%
      # make bins based on trace length
      mutate(bin = ntile(trace_length, trace_length_bins)) %>%
      # for each bin, sample the fraction of the event log according to split
      group_by(bin) %>%
      sample_frac(split) %>%
      # pull the case ids from these fractions
      pull(ith_case) -> unique_cases_train

    train_df <-  processed_df %>% filter(ith_case %in% unique_cases_train)
    test_df <- processed_df %>% filter(!(ith_case %in% unique_cases_train))

    list(train_df = train_df, test_df = test_df)

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

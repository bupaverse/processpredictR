#' Tokenize features and target of a preprocessed [`data.frame`]
#'
#' @description Tokenize features and target of a processed [`data.frame`] to fit the transformer model.
#'
#' @inheritParams split_train_test
#' @param vocabulary [`list`]: A vocabulary, i.e. a list of activities and outcome labels.
#'
#' @return A list of tokens: token_x and token_y
#'
#' @examples
#' library(eventdataR)
#' ex_remaining_trace <- prepare_examples(patients, task = "remaining_trace")
#' tokenize(ex_remaining_trace, vocabulary = create_vocabulary(ex_remaining_trace))
#'
#' @export
tokenize <- function(processed_df) {
  UseMethod("tokenize")
}

#' @export
tokenize.ppred_examples_df <- function(processed_df) {

  # vocabulary and task
  vocabulary <- attr(processed_df, "vocabulary")
  task <- attr(processed_df, "task")

  # algorithm to produce token_x (same for all tasks)
  token_x <- list()

  for (i in (1:length(processed_df$prefix))) {
    #case_trace <- list()
    case_trace <- c()

    for (j in (1:length(processed_df$prefix_list[[i]]))) {
      #if (processed_df$trace[[i]][j] == x_word_dict$values_x) {}
      tok <- which(processed_df$prefix_list[[i]][j] == vocabulary$keys_x)
      case_trace <- case_trace %>% append(tok-1)
    }

    case_trace <- case_trace %>% list()
    token_x <- token_x %>% append(case_trace)
  }


  # time_x (extra features)
  time_x <- NULL
  if (!is.null(attr(processed_df, "features"))) {
    time_x <- processed_df %>%
      as_tibble() %>%
      select(attr(processed_df, "numeric_features")) %>%
      as.list() %>%
      purrr::map(scale) %>%
      purrr::map(as.vector) %>%
      as_tibble()

    cat_features <- processed_df %>%
      as_tibble() %>%
      select(attr(processed_df, "hot_encoded_categorical_features"))

    time_x <- time_x %>% cbind(cat_features) %>% data.matrix()


    #num_feats <- time_x %>% purrr::map_if(is.numeric, scale) %>% as.vector() %>% as_tibble() %>% select(is.numeric)
    # cat_feats <- time_x %>% data.table::as.data.table() %>% select(is.factor)
    #
    # if (length(cat_feats) > 0) {
    #   cat_feats <- cat_feats %>% mltools::one_hot()
    #   num_cat_feats <- num_feats %>% cbind(cat_feats)
    #   time_x <- num_cat_feats %>% data.matrix()
    #
    #   # number of features increases
    #   number_features <- num_cat_feats %>% length
    # }
    # else {
    #   time_x <- num_feats %>% data.matrix()
    # }

    # purrr::map(time_x)
    # newdata <- reshape2::dcast(data = tmp, handling_id ~ employee, length)

    # feats <- time_x %>%
    #   purrr::map_if(is.numeric, scale, .else = ~caret::dummyVars("~.", ., fullRank = TRUE)) %>%
    #   purrr::map(as.vector) %>%
    #   purrr::map_if(is.numeric, ~append(feats), ~append(predict(feats, time_x)))
    #
    # for (i in 1:length(time_x)) {
    #   if (is.numeric(time_x[i])) {
    #     scale(time_x[i]) %>% as.vector()
    #   }
    #   else {
    #     #caret::dummyVars("~.", ., fullRank = TRUE))
    #     cat_feats <- append(time_x[i])
    #   }
    # }
  }

  #algorithm to produce token_y

  # OUTCOME or NEXT_ACTIVITY
  if (task == "outcome") {

    # token_y for OUTCOME
    token_y = c()

    for (i in (1:length(processed_df$prefix_list))) {
      tok <- which(processed_df$outcome[i] == vocabulary$keys_y) #match outcome instead of trace
      token_y <- token_y  %>% append(tok-1)
    }

    # return a list of tokens
    tokens <- list(token_x = token_x, time_x = time_x, token_y = token_y)
    class(tokens) <- c("ppred_examples_tokens", "list")
    tokens
  }

  # token_y for NEXT_ACTIVITY
  else if (task == "next_activity") {
    token_y = c()

    for (i in (1:length(processed_df$prefix))) {
      tok <- which(processed_df$next_activity[i] == vocabulary$keys_y) #match outcome instead of trace
      token_y <- token_y  %>% append(tok-1)
    }

    # return a list of tokens
    tokens <- list(token_x = token_x, time_x = time_x, token_y = token_y)
    class(tokens) <- c("ppred_examples_tokens", "list")
    tokens

  }

  # token_y for REMAINING_TRACE
  else if (task == "remaining_trace") {
    token_y = c()

    for (i in (1:length(processed_df$prefix))) {
      tok <- which(processed_df$remaining_trace[i] == vocabulary$keys_y)
      token_y <- token_y  %>% append(tok-1)
    }

    # return a list of tokens:
    tokens <- list(token_x = token_x, time_x = time_x, token_y = token_y)
    class(tokens) <- c("ppred_examples_tokens", "list")
    tokens
  }

  # NEXT_TIME and REMAINING_TIME tasks
  else if (task %in% c("next_time", "remaining_time")) {

    # inversing times back to interpret the model predictions output
    # time_passed1 * attr(time_passed1, 'scaled:scale') + attr(time_passed1, 'scaled:center'))

    # # time_x (input)
    # time_x <- processed_df %>% as_tibble() %>% select(attr(processed_df, "features")) %>% as.list() %>%
    #   purrr::map(scale) %>%
    #   purrr::map(as.vector)
    # recent_time <- processed_df$recent_time %>% scale() %>% as.vector()
    # latest_time <- processed_df$latest_time %>% scale() %>% as.vector()
    # time_passed <- processed_df$time_passed %>% scale() %>% as.vector()
    # time_x <- list(recent_time = recent_time, latest_time = latest_time, time_passed = time_passed)

    #time_y (output)
    time_y <- processed_df %>% as_tibble() %>% select(attr(processed_df, "y_var")) %>% #as.list() %>%
      scale() %>%
      as.vector()
      # purrr::map(scale) %>%
      # purrr::map(as.vector)

    # return:
    # token_x, i.e. activity prefixes
    # time_x, i.e. a list of calculated and scaled durations (recent, latest, passed)
    # time_y, i.e. a next_activity duration
    tokens <- list(token_x = token_x, time_x = time_x, token_y = time_y)
    class(tokens) <- c("ppred_examples_tokens", "list")
    tokens
  } # followed by train_token_x %>% reticulate::np_array(dtype = "float32") in transformer_fit or predict

  # else if (task == "remaining_time") {
  #
  #   # later alternatively inversing times back to interpret the model predictions output
  #   # time_passed1 * attr(time_passed1, 'scaled:scale') + attr(time_passed1, 'scaled:center'))
  #
  #   # time_x (input)
  #   time_x <- processed_df %>% select(throughput_time, processing_time, previous_duration) %>% as.list() %>%
  #     purrr::map(scale) %>%
  #     purrr::map(as.vector)
  #   # recent_time <- processed_df$recent_time %>% scale() %>% as.vector()
  #   # latest_time <- processed_df$latest_time %>% scale() %>% as.vector()
  #   # time_passed <- processed_df$time_passed %>% scale() %>% as.vector()
  #   # time_x <- list(recent_time = recent_time, latest_time = latest_time, time_passed = time_passed)
  #
  #   #time_y (output)
  #   time_y <- processed_df$remaining_time %>% scale() %>% as.vector()
  #
  #   # return:
  #   # token_x, i.e. activity prefixes
  #   # time_x, i.e. a list of calculated and scaled durations (recent, latest, passed)
  #   # time_y, i.e. a next_activity duration
  #
  #   # class(token_x) <- c("token", "list")
  #   # class(recent_time) <- c("tokens_time", "list")
  #   # class(latest_time) <- c("tokens_time", "list")
  #   # class(time_passed) <- c("tokens_time", "list")
  #   # class(time_y) <- c("tokens_time", "token", "list")
  #
  #   tokens <- list(token_x = token_x, time_x = time_x, token_y = time_y)
  #   class(tokens) <- c("tokens", "list")
  #   tokens
  # }

}



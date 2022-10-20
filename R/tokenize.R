#' Tokenize prefix (feature) and outcome (target) of a processed dataframe to fit the transformer model
#'
#' (WIP)
#'
#'
#' @param processed_df A processed dataframe (= processed event log)
#' @return A list of tokens token_x and token_y
#'
#' @examples
#' library(eventdataR)
#' testtime <- create_prefix_df(patients, prediction = "next_time")
#' testout <- create_prefix_df(patients, prediction = "outcome")
#' testact <- create_prefix_df(patients, prediction = "next_activity")
#' purrr::map(list(testtime, testout,testact), tokenize) %>% head(10)
#'
#'@export
tokenize <- function(processed_df) {
  #activities <- as.character(activity_labels(log))

  # OUTCOME task
  if ("outcome" %in% names(processed_df)) { #temporarily works only if there is a column named outcome (after preprocess_log())

    activities <- processed_df$current_activity %>% unique() %>% as.character()
    outcomes <- processed_df$outcome %>% unique() %>% as.character()
    values_x <- activities %>% append(outcomes) %>% unique()

  }

  # NEXT_TIME task
  else if ("next_time" %in% names(processed_df)) {

    activities <- processed_df[[bupaR::activity_id(processed_df)]] %>% as.character() %>% unique()
    values_x <- activities

  }

  # NEXT_ACTIVITY task
  #else if ("next_activity" %in% names(processed_df) && !("outcome" %in% names(processed_df))) {
  else {

    activities <- processed_df$current_activity %>% append(processed_df$next_activity) %>% unique()
    values_x <- activities
    outcomes <- processed_df$next_activity %>% unique()

  }

  # Get traces as lists from prefix
  processed_df <- processed_df %>%
    mutate(trace = str_split(prefix, pattern = " - "))

  #input tokens
  values_x <- c("PAD", "UNK") %>%
    append(values_x)

  keys_x <- as.list(values_x)
  keys_x


  #algorithm to produce token_x (same for all tasks)
  token_x <- list()

  for (i in (1:length(processed_df$trace))) {
    #case_trace <- list()
    case_trace <- c()

    for (j in (1:length(processed_df$trace[[i]]))) {
      #if (processed_df$trace[[i]][j] == x_word_dict$values_x) {}
      tok <- which(processed_df$trace[[i]][j] == keys_x)
      case_trace <- case_trace %>% append(tok-1)
    }

    case_trace <- case_trace %>% list()
    token_x <- token_x %>% append(case_trace)

  }



  #algorithm to produce token_y

  # OUTCOME or NEXT_ACTIVITY
  if ("outcome" %in% names(processed_df) || ("next_activity" %in% names(processed_df) && !("outcome" %in% names(processed_df)))) {
    #outcome tokens
    keys_y <- outcomes %>% as.character() %>% as.list()
    keys_y

    # token_y for OUTCOME
    if ("outcome" %in% names(processed_df)) {
      token_y = c()

      for (i in (1:length(processed_df$trace))) {
        tok <- which(processed_df$outcome[i] == keys_y) #match outcome instead of trace
        token_y <- token_y  %>% append(tok-1)

      }

      #list(token_x = token_x, token_y = token_y)

    }


    # token_y for NEXT_ACTIVITY
    else if ("next_activity" %in% names(processed_df)) {
      token_y = c()

      for (i in (1:length(processed_df$trace))) {
        tok <- which(processed_df$next_activity[i] == keys_y) #match outcome instead of trace
        token_y <- token_y  %>% append(tok-1)
      }

      #list(token_x = token_x, token_y = token_y)

    }

    # return a list of tokens
    list(token_x = token_x, token_y = token_y)

  }


  # NEXT_TIME task
  else if ("next_time" %in% names(processed_df)) {

    # inversing times back to interpret the model predictions output
    # time_passed1 * attr(time_passed1, 'scaled:scale') + attr(time_passed1, 'scaled:center'))

    # time_x (input)
    recent_time <- processed_df$recent_time %>% scale() %>% as.vector()
    latest_time <- processed_df$latest_time %>% scale() %>% as.vector()
    time_passed <- processed_df$time_passed %>% scale() %>% as.vector()
    time_x <- list(recent_time = recent_time, latest_time = latest_time, time_passed = time_passed)

    #time_y (output)
    time_y <- processed_df$next_time %>% scale() %>% as.vector()

    # return:
    # token_x, i.e. activity prefixes
    # time_x, i.e. a list of calculated and scaled durations (recent, latest, passed)
    # time_y, i.e. a next_activity duration
    list(token_x = token_x, time_x = time_x, token_y = time_y)

  } # followed by train_token_x %>% reticulate::np_array(dtype = "float32") in transformer_fit or predict

}


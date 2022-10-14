#' Tokenize prefix (feature) and outcome (target) of a processed dataframe to fit the transformer model
#'
#' (WIP)
#'
#'
#' @param processed_df A processed dataframe (= processed event log)
#' @return A list of tokens token_x and token_y
#'
#'@export
tokenize <- function(processed_df) {
  #activities <- as.character(activity_labels(log))

  #if column exists
  if ("outcome" %in% names(processed_df)) { #temporarily works only if there is a column named outcome (after preprocess_log())
    activities <- processed_df$current_activity %>% unique() %>% as.character()
    outcomes <- processed_df$outcome %>% unique() %>% as.character()
    values_x <- activities %>% append(outcomes) %>% unique()

  }

  else if ("next_activity" %in% names(processed_df)) {
    activities <- processed_df$current_activity %>% append(processed_df$next_activity) %>% unique()
    values_x <- activities
    outcomes <- processed_df$next_activity %>% unique()


  }

  processed_df <- processed_df %>%
    mutate(trace = str_split(prefix, pattern = " - "))

  #input tokens
  values_x <- c("PAD", "UNK") %>%
    append(values_x)

  keys_x <- as.list(values_x)
  keys_x

  #outcome tokens
  keys_y <- outcomes %>% as.character() %>% as.list()
  keys_y



  #algorithm to produce token_x
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

  if ("outcome" %in% names(processed_df)) {
    token_y = c()

    for (i in (1:length(processed_df$trace))) {
      tok <- which(processed_df$outcome[i] == keys_y) #match outcome instead of trace
      token_y <- token_y  %>% append(tok-1)

    }

    list(token_x = token_x, token_y = token_y)

  }

  else if ("next_activity" %in% names(processed_df)) {
    token_y = c()

    for (i in (1:length(processed_df$trace))) {
      tok <- which(processed_df$next_activity[i] == keys_y) #match outcome instead of trace
      token_y <- token_y  %>% append(tok-1)
    }

    list(token_x = token_x, token_y = token_y)


  }

}


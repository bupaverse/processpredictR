#' Tokenize prefix (feature) and outcome (target) of a processed dataframe to fit the transformer model
#'
#' (WIP)
#'
#'
#' @param processed_df A processed dataframe (= processed event log)
#' @param activities A character vector of unique activity labels from an event log
#' @return A list of tokens token_x and token_y
#'
#'@export
tokenize <- function(processed_df, activities) {
  #activities <- as.character(activity_labels(log))
  outcomes <- processed_df$outcome %>% unique()

  processed_df <- processed_df %>%
    mutate(trace = str_split(prefix, pattern = " - "))

  #input tokens
  values_x <- activities %>% append(as.character(outcomes))
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
  token_y = c()

  for (i in (1:length(processed_df$trace))) {
    tok <- which(processed_df$outcome[i] == keys_y) #match outcome instead of trace
    token_y <- token_y  %>% append(tok-1)
  }

  list(token_x = token_x, token_y = token_y)

}

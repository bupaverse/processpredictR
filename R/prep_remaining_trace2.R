#' Prepares processed event log of class `ppred_examples_df` for another variation of remaining trace prediction
#'
#' This variation includes as well encoder as decoder block.
#'
#' @param log \code{\link{log}}: Object of class \code{\link{log}} or derivatives (\code{\link{grouped_log}}, \code{\link{eventlog}},
#' \code{\link{activitylog}}, etc.).
#'
#' @export
prep_remaining_trace2 <- function(log) {
  UseMethod("prep_remaining_trace2")
}

#' @export
prep_remaining_trace2.ppred_examples_df <- function(log) {
  log$remaining_trace_list <- log$remaining_trace_list %>% purrr::map(~append("startpoint", .))
  vocabulary <- log %>% attr("vocabulary")
  vocabulary$keys_x <- vocabulary$keys_x %>% append(list("endpoint")) %>% append(list("startpoint"))

  # prefix_list to tokens
  token_x <- list()
  for (i in (1:nrow(log))) {
    #case_trace <- list()
    case_trace <- c()

    for (j in (1:length(log$prefix_list[[i]]))) {
      #if (processed_log$trace[[i]][j] == x_word_dict$values_x) {}
      tok <- which(log$prefix_list[[i]][j] == vocabulary$keys_x)
      case_trace <- case_trace %>% append(tok-1)
    }

    case_trace <- case_trace %>% list()
    token_x <- token_x %>% append(case_trace)
  }

  # remaining_trace_list to tokens
  token_y <- list()
  for (i in (1:nrow(log))) {
    #case_trace <- list()
    case_trace <- c()

    for (j in (1:length(log$remaining_trace_list[[i]]))) {
      #if (processed_log$trace[[i]][j] == x_word_dict$values_x) {}
      tok <- which(log$remaining_trace_list[[i]][j] == vocabulary$keys_x)
      case_trace <- case_trace %>% append(tok-1)
    }

    case_trace <- case_trace %>% list()
    token_y <- token_y %>% append(case_trace)
  }

  # shift remaining_tokens for training
  remaining_tokens_shifted <- token_y %>% purrr::map_depth(.depth = 1, lead, n=1, default = 0)
  remaining_tokens_shifted <- remaining_tokens_shifted %>% keras::pad_sequences(padding = "post")
  # remaining_tokens
  remaining_tokens <- token_y %>% keras::pad_sequences(padding = "post")
  # current_tokens
  current_tokens <- token_x %>% keras::pad_sequences()



  # # remaining trace list containing tokenized sequences with some mapped attributes
  # remaining_trace <- list(log = log, current_tokens = current_tokens, remaining_tokens = remaining_tokens,
  #                        remaining_tokens_shifted = remaining_tokens_shifted)
  remaining_trace <- list()
  class(remaining_trace) <- c("remaining_trace2", class(remaining_trace))
  remaining_trace$log <- log
  remaining_trace$current_tokens <- current_tokens
  remaining_trace$remaining_tokens <- remaining_tokens
  remaining_trace$remaining_tokens_shifted <- remaining_tokens_shifted

  # attributes to each tokenized sequence
  attr(remaining_trace, "task") <- "remaining_trace2"
  attr(remaining_trace, "vocabulary") <- vocabulary$keys_x
  attr(remaining_trace, "vocab_size") <- vocabulary$keys_x %>% length()
  attr(remaining_trace, "input_maxlen") <- remaining_trace$current_tokens %>% ncol()
  attr(remaining_trace, "target_maxlen") <- remaining_trace$remaining_tokens %>% ncol()

  return(remaining_trace)
}




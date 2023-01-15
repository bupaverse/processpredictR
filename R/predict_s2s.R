

predict_s2s <- function(object, test_data, output = c("append", "y_pred", "raw"), ...) {


  pred_trace <- NULL

  vocab <- object$vocabulary

  token_x <- purrr::list_along(1:nrow(test_data))
  for (i in (1:nrow(test_data))) {
    #case_trace <- list()
    case_trace <- numeric(length(test_data$prefix_list[[i]]))

    for (j in (1:length(test_data$prefix_list[[i]]))) {
      #if (processed_log$trace[[i]][j] == x_word_dict$values_x) {}
      tok <- which(test_data$prefix_list[[i]][j] == vocab$keys_x) - 1
      case_trace[j] <- tok
    }

    token_x[[i]] <- case_trace
  }

  current_tokens <- token_x %>% keras::pad_sequences(maxlen = attr(test_data, "input_maxlen"))


  prediction_toks <- as.list(rep(which(vocab$keys_x == "startpoint")-1, times = nrow(current_tokens))) %>%
    keras::pad_sequences(maxlen = attr(test_data, "target_maxlen"), padding = "post")

  to_predict <- list(current_tokens, prediction_toks)

    finished <- list()
  i <- 1
  pb <- progress::progress_bar$new(total = attr(test_data, "target_maxlen")-1)
    while(i < attr(test_data, "target_maxlen")) {
    pb$tick()
    # apply model
      object$model(list(to_predict), training = F) -> pred

    pred[,i,] %>%
      keras::k_argmax() -> predicted_tokens

    prediction_toks[,i+1] <- as.matrix(predicted_tokens)

    to_predict <- list(current_tokens, prediction_toks)
    i <- i + 1
  }


  prediction_toks %>%
    purrr::map(~vocab$keys_x[[.x+1]]) %>%
    matrix(nrow = nrow(prediction_toks)) %>%
    as_tibble(.name_repair = "minimal") %>%
    setNames(paste0("V", 1:attr(test_data, "target_maxlen"))) %>%
    unnest(cols = everything()) %>%
    unite(col = "pred_trace", sep = " - ") %>%
    mutate(pred_trace = str_remove_all(pred_trace, " - PAD")) %>%
    mutate(pred_trace = str_remove(pred_trace, "startpoint - ")) -> pred

  if(output == "append") {
    test_data %>%
      mutate(pred_remaining_trace = pred$pred_trace)
  } else if(output == "raw") {
    prediction_toks
  } else if (output == "y_pred")
    pred$pred_trace
}

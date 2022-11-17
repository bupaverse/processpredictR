#' Define transformer model
#'
#' (WIP)
#'
#' @param processed_df A processed [`data.frame`] from prepare_examples
#' @return A transformer model
#'
#' @export
create_model <- function(processed_df) {
  UseMethod("create_model")
}

#' @export
create_model.ppred_examples_df <- function(processed_df) {

  # tf <- import("tensorflow")
  # layers <- import("keras")$layers
  # activations <- import("keras")$activations
  #reticulate::py_run_file(system.file("python", "your_script.py", package = "yourpkg"))

  #reticulate::repl_python() # opens python console
  # if(prediction == "next_activity") {
  #   maxlen <- max_case_length(log) + 1 %>% as.integer()
  # }

  # vocabulary and task
  vocabulary <- get_vocabulary(processed_df)
  task <- get_task(processed_df)

  # parameters of the model
  maxlen <- max_case_length(processed_df)
  vocab_size <- vocab_size(processed_df)
  num_features <- processed_df %>% attr("features") %>% length()


  # OUTCOME and NEXT_ACTIVITY tasks
  if (task %in% c("outcome", "next_activity")) {

    num_output <- num_outputs(processed_df)

    # same for both OUTCOME and NEXT_ACTIVITY
    source_python("inst/transformer_model.py")
    get_outcome_transformer_model(maxlen, vocab_size, num_output)
  }

  else if (task == "remaining_trace") {

    num_output <- num_outputs(processed_df)

    source_python("inst/transformer_model.py")
    get_remaining_trace_model(maxlen, vocab_size, num_output)
  }

  else if (task == "next_time") {

    source_python("inst/transformer_model.py")
    get_next_time_model(maxlen, num_features, vocab_size, as.integer(1))
  }

  else if (task == "remaining_time") {

    source_python("inst/transformer_model.py")
    get_remaining_time_model(maxlen, num_features, vocab_size, as.integer(1))
  }
}






#' Define transformer model
#'
#' (WIP)
#'
#' @param processed_df [`data.frame`]: A processed [`data.frame`] from prepare_examples
#' @param custom_model_py [`character`] (default [`"default"`]): A default or a custom .py model
#' @param name [`character`]: A name for the model
#' @return A transformer model
#'
#' @export
create_model <- function(processed_df, custom_model_py = c("default", "custom"), name = NULL) {
  UseMethod("create_model")
}

#' @export
create_model.ppred_examples_df <- function(processed_df, custom_model_py = c("default", "custom"), name = NULL) {

  # tf <- import("tensorflow")
  # layers <- import("keras")$layers
  # activations <- import("keras")$activations
  #reticulate::py_run_file(system.file("python", "your_script.py", package = "yourpkg"))

  #reticulate::repl_python() # opens python console
  # if(prediction == "next_activity") {
  #   maxlen <- max_case_length(log) + 1 %>% as.integer()
  # }


  custom_model_py <- rlang::arg_match(custom_model_py)

  # vocabulary and task
  vocabulary <- get_vocabulary(processed_df)
  task <- get_task(processed_df)

  # parameters of the model
  maxlen <- max_case_length(processed_df)
  vocab_size <- vocab_size(processed_df)
  # if (is.null(custom_model_py)) { # model is custom if the user passed keras:: functions to custom_model_py
  #   custom <- "default"
  # }
  # else custom <- "custom"

  custom <- custom_model_py
  name <- task

  if (!is.null(attr(processed_df, "features"))) {
    num_features <- processed_df %>% attr("features") %>% length()
  }
  else {
    num_features <- 0
  }

  source_python("inst/transformer_model.py")

  if (task %in% c("outcome", "next_activity")) {

    num_output <- num_outputs(processed_df)
    model <- get_outcome_transformer_model(maxlen, num_features, vocab_size, num_output, name, custom)
  }

  else if (task == "remaining_trace") {

    num_output <- num_outputs(processed_df)
    model <- get_remaining_trace_model(maxlen, num_features, vocab_size, num_output, name, custom)
  }

  else if (task == "next_time") {

    model <- get_next_time_model(maxlen, num_features, vocab_size, as.integer(1), name, custom)
  }

  else if (task == "remaining_time") {

    model <- get_remaining_time_model(maxlen, num_features, vocab_size, as.integer(1), name, custom)
  }

  attr(model, "max_case_length") <- maxlen
  attr(model, "features") <- processed_df %>% attr("features")
  attr(model, "num_features") <- num_features
  class(model) <- c("ppred_model", class(model))
  return(model)

}






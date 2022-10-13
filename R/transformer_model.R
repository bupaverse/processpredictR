#' Define transformer model
#'
#' (WIP)
#'
#' @param processed_df A processed dataframe from create_prefix_df
#' @return A transformer model
#'
#' @export
transformer_model <- function(processed_df) {

  #@param max_case_length An integer number which is the maximum trace length in an event log

  # tf <- import("tensorflow")
  # layers <- import("keras")$layers
  # activations <- import("keras")$activations
  #reticulate::py_run_file(system.file("python", "your_script.py", package = "yourpkg"))

  #reticulate::repl_python() # opens python console

  # if(prediction == "next_activity") {
  #
  #   maxlen <- max_case_length(log) + 1 %>% as.integer()
  #
  # }

  maxlen <- max_case_length(processed_df)
  vocab_size <- vocab_size(processed_df)
  num_output <- num_outputs(processed_df)


  source_python("inst/transformer_model.py")
  get_outcome_transformer_model(maxlen, vocab_size, num_output)

}

#' Define transformer model
#'
#' (WIP)
#'
#' @param max_case_length An integer number which is the maximum trace length in an event log
#' @param vocab_size An integer number which is the number of unique activities, output labels and padding keys ("PAD", "UNK")
#' @param num_output An integer number of output labels
#' @return A transformer model
#'
#' @export
transformer_model <- function(max_case_length, vocab_size, num_output) {

  tf <- import("tensorflow")
  layers <- import("keras")$layers
  activations <- import("keras")$activations

  source_python("transformer_model.py")
  get_outcome_transformer_model(max_case_length, vocab_size, num_output)

}

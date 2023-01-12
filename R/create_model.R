#' Define transformer model
#'
#' Defines the model using the keras functional API.
#' The following 4 process monitoring tasks are defined:
#' * outcome
#' * next_activity
#' * next_time
#' * remaining_time
#' * remaining_trace
#' * remaining_trace_s2s
#'
#' @param x_train [`data.frame`]: A processed [`data.frame`] from [prepare_examples()].
#' @param custom [`logical`] (default [`FALSE`]): If [`TRUE`], returns a custom model.
#' @param ... you can pass additional arguments to `keras::keras_model()` (ex.: `name` argument).
#' @return An object of class [`ppred_model`] and [`list`] containing a Transformer model (returned by `keras::keras_model()`) and some additional useful metrics.
#'
#' @export
create_model <- function(x_train, custom = FALSE, ...) {
  UseMethod("create_model")
}

#' @export
create_model.ppred_examples_df <- function(x_train, custom = FALSE, ...) {

  if (attr(x_train, "task") != "remaining_trace_s2s") {
    create_model_original.ppred_examples_df(x_train, custom = custom, ...)
  }

  else {
    x_train <- prep_remaining_trace2.ppred_examples_df(x_train)
    create_model_s2s.remaining_trace_s2s(x_train, custom = custom, ...)
  }
}


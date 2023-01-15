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
#' @param num_heads A number of attention heads of the [keras::layer_embedding()].
#' @param output_dim_emb Dimension of the dense embedding of the [keras::layer_embedding()].
#' @param dim_ff Dimensionality of the output space of the feedforward network part of the model (`units` argument of the [keras::layer_dense()]).
#' @param ... you can pass additional arguments to `keras::keras_model()` (ex.: `name` argument).
#' @return An object of class [`ppred_model`] and [`list`] containing a Transformer model (returned by `keras::keras_model()`) and some additional useful metrics.
#'
#' @export
create_model <- function(x_train, custom = FALSE, num_heads = 4, output_dim_emb = 36, dim_ff = 64, ...) {
  UseMethod("create_model")
}

#' @export
create_model.ppred_examples_df <- function(x_train, custom = FALSE, num_heads = 4, output_dim_emb = 36, dim_ff = 64, ...) { #num_heads, d_model, dff

  if (attr(x_train, "task") != "remaining_trace_s2s") {
    create_model_original(x_train, custom = custom, num_heads = num_heads, embed_dim = output_dim_emb, ff_dim = dim_ff, ...)
  }

  else {
    if (custom == TRUE) {
      cli::cli_alert_warning("No custom architecture is made available for the task remaining_trace_s2s.")
      cli::cli_alert_info("a full model architecture is supplied.")
    }

    #x_train <- prep_remaining_trace2(x_train)
    create_model_s2s(x_train, num_heads = num_heads, d_model = output_dim_emb, dff = dim_ff, ...)
  }
}


assign_outcome_labels <- function(prefix_df, ...) {
  prefix_df %>%
    mutate(outcome = forcats::fct_collapse(last_activity, ...))
}



evaluate_s2s <- function(model, test_data, ...) {

  tmp <- predict(model, test_data, output = "append")

  tibble("categorical_accuracy" = sum(tmp$pred_remaining_trace == tmp$remaining_trace)/nrow(tmp))

}

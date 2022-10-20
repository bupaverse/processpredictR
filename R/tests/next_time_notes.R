# Next time prediction ----------------------------------------------------
# Use bupar and lubridate to work with dates and times
library(ggplot2)
library(eventdataR)
create_prefix_df(patients, prediction = "outcome")



# time_x to np_array ------------------------------------------------------

tokens_train$time_x$recent_time %>% reticulate::np_array(dtype = "float32") %>% reticulate::array_reshape(dim = c(-1, 1))
tokens_train$time_x$latest_time %>% reticulate::np_array(dtype = "float32") %>% reticulate::array_reshape(dim = c(-1, 1))
tokens_train$time_x$time_passed %>% reticulate::np_array(dtype = "float32") %>% reticulate::array_reshape(dim = c(-1, 1))

as.array(c(reticulate::np_array(tokens_train$time_x$recent_time, dtype="float32"), reticulate::np_array(tokens_train$time_x$latest_time)))

append(tokens_train$time_x$recent_time %>% reticulate::np_array(dtype = "float32") %>% reticulate::array_reshape(dim = c(-1, 1)),
       tokens_train$time_x$latest_time %>% reticulate::np_array(dtype = "float32") %>% reticulate::array_reshape(dim = c(-1, 1)))




tokens_train$time_x %>% names()



tokens_train$token_y  %>% reticulate::np_array(dtype = "float32")

# SOLUTION
matrix(c(tokens_train$time_x$recent_time, tokens_train$time_x$latest_time, tokens_train$time_x$time_passed), ncol = 3) %>%
  reticulate::np_array(dtype = "float32")

tokens_train$token_x %>% keras::pad_sequences(maxlen = max_case_length(df), value = 0)
tokens_train$token_x %>% keras::pad_sequences(maxlen = max_case_length(df), value = 0) %>% reticulate::np_array(dtype = "float32")





# SCALE TIME DURATIONS ----------------------------------------------------
scale(df_test$next_time) -> standardScaled
standardScaled
scales::rescale(test$next_time, to = c(0, 1)) -> minmaxScaled

library(scales)
standardScaled * attr(standardScaled, 'scaled:scale') + attr(standardScaled, 'scaled:center')

x[995:1001] == test$next_time[995:1001]

x[995:1001]
test$next_time[995:1001]


# Scenario1 ---------------------------------------------------------------
# patients %>% bupaR:::group_by_ids(activity_id, case_id)
# distribution of activity durations
to_activitylog(patients) %>%
  mutate(activity_duration = complete-start) %>% ggplot(aes(handling, activity_duration)) + geom_boxplot()

to_activitylog(patients) %>% # situation when there is both start- and end timestamps
  group_by(!!bupaR:::case_id_(patients)) %>%
  mutate(activity_duration = (complete-start) %>% as.numeric(),
         time_passed = cumsum(activity_duration),
         trace = paste(handling, collapse = ",")) %>% # getting the traces
  arrange(!!bupaR:::case_id_(patients)) %>% select(activity_duration, time_passed, trace, everything()) %>%
  mutate(!!bupaR:::case_id_(patients),
            prefix = purrr::accumulate(as.character(!!bupaR:::activity_id_(patients)), paste, sep = " - "),
            !!bupaR:::activity_id_(patients),
            activity_duration = activity_duration,
            time_passed = time_passed) -> test


# WHAT TO DO WITH NA's ?
test %>%
  mutate(latest_time = activity_duration,
         next_time = lead(activity_duration),
         recent_time = lag(activity_duration)
         ) %>% select(latest_time, time_passed, next_time, recent_time, activity_duration, handling, patient)








# Scenario2 ---------------------------------------------------------------
sepsis %>% to_activitylog() # situation when there is no start timestamp or end timestamp -> define algorithm for activity duration







traces_per_case <- case_list(patients, .keep_trace_list = TRUE) %>%
  mutate(ith_case = row_number()) %>%
  rename(case_id = case_id(patients))
traces_per_case


case_prefix <- traces_per_case %>%
  group_by(ith_case, case_id) %>%
  #group_by(across(c(5, 1))) %>%
  summarise(prefix = purrr::accumulate(traces_per_case$trace_list[[ith_case]], paste, sep = " - "),
            current_activity = traces_per_case$trace_list[[ith_case]],
            traces = traces_per_case$trace_list[ith_case]) %>%
  mutate(k = row_number() - 1,
         outcome = last(current_activity),
         next_activity = lead(current_activity)) %>%
  filter(!is.na(next_activity)) %>%
  ungroup()

case_prefix <- case_prefix %>% assign_outcome_labels(...)
case_prefix






















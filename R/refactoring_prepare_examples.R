traces_per_case <- case_list(patients, .keep_trace_list = TRUE)

patients %>%
  left_join(traces_per_case, by = case_id(patients)) %>%
  # if start is missing, set equal to complete
  # if complete is missing, set equal to start
  group_by_case() %>%
  mutate(ith_case = cur_group_id(),
         k = row_number() - 1,
         current_activity = !!bupaR:::activity_id_(patients),
         prefix_list = purrr::accumulate(as.character(current_activity), c),
         prefix = purrr::accumulate(as.character(current_activity), paste, sep = " - ")) %>%
  arrange(!!bupaR:::case_id_(patients)) %>%
  distinct(!!bupaR:::activity_instance_id_(patients), .keep_all = T) %>%
  mutate(
    next_activity = lead(current_activity),
    next_activity = if_else(is.na(next_activity), "endpoint", as.character(next_activity)),
    outcome = last(current_activity)) %>%
  select(ith_case, !!bupaR:::case_id_(patients), prefix_list, prefix, outcome, k, current_activity,
         everything(), -trace_id, -trace, -current_activity) %>%
  #assign_outcome_labels(...) %>%
  re_map(mapping = mapping(patients)) -> output

library(data.table)


patients %>% as.data.table() %>%
  .[, ith_case := .GRP, by = patient] %>%
  .[, `:=`(k = rowid(ith_case) - 1),
    current_activity = ] %>% View




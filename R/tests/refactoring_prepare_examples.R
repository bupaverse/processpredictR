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
library(bupaverse)

patients %>% as.data.table() %>%
  .[, ith_case := .GRP, by = patient] %>%
  .[, `:=`(k = rowid(ith_case) - 1),
    current_activity = ] %>% View

traces_per_case <- case_list(patients, .keep_trace_list = TRUE)
traces_per_case_dt <- as.data.table(traces_per_case)
log_dt <- as.data.table(patients)

data <- merge(log_dt, traces_per_case_dt, by = case_id(patients))

data[,`:=`("current_activity" = handling,
           "prefix_list"= purrr::accumulate(as.character(handling), c),
           "prefix"= purrr::accumulate(as.character(handling), paste, sep = " - ")),.(patient)]
data$prefix

features <- c("employee")



patients %>%
  select(activity_instance_id(patients), any_of(features), force_df = T) %>%
  distinct() -> feature_data

log <- patients

log_dt <- log %>%
  rename(AID = activity_id(log),
         CID = case_id(log),
         AIID = activity_instance_id(log),
         TS = timestamp(log)) %>%
  as.data.table() %>%
  mutate(AID = as.character(AID))

log_dt[,
       .('start_time' = min(TS),
         'end_time' = max(TS),
         'min_order' = min(.order)),
       by = .(AID, AIID, CID)][order(start_time, end_time, min_order),
                               c("ith_case", "k", "current_activity","prefix_list", "prefix", "next_activity", "outcome") := .(
                                 .GRP,
                                 (1:.N)-1,
                                 AID,
                                 purrr::accumulate(as.character(AID), c),
                                 purrr::accumulate(as.character(AID), paste, sep = " - "),
                                 shift(AID, n = -1, fill = "endpoint"),
                                 last(AID)),
                               .(CID)] -> new_log_dt
new_log_dt;new_log_dt










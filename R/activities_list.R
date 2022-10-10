#'Retrieve list of event log activities
#'
#'(maybe not export, for internal use only)
#'
#'@param log An object of class log
#'
#'@export
activities_list <- function(log) {
  UseMethod("activities_list")
}

#'@export
activities_list.log <- function(log) {
  activities <- as.character(activity_labels(log))
  activities
}

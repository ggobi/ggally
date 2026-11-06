crosstalk_key <- function() {
  ".crossTalkKey"
}

fortify_SharedData <- function(model, data, ...) {
  key <- model$key()
  set <- model$groupName()
  data <- model$origData()
  # need a consistent name so we know how to access it in ggplotly()
  # MUST be added last. can NOT be done first
  data[[crosstalk_key()]] <- key
  structure(data, set = set)
}

fix_data_slim <- function(data, isSharedData) {
  if (isSharedData) {
    data[[crosstalk_key()]] <- NULL
  }
  data
}

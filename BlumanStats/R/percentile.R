percentile <-
function(data, p) {
  data <- sort(data)  # Sort the data
  n <- length(data)
  index <- p * n
  
  if (index %% 1 == 0) {
    # If the index is a whole number, take the observation at that location
    percentile_value <- (data[ceiling(index)] + data[ceiling(index) + 1]) / 2
  } else {
    # If the index is not a whole number, round up and take the observation at that location
    percentile_value <- data[ceiling(index)]
  }
  
  return(percentile_value)
}

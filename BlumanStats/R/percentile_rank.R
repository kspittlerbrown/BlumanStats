percentile_rank <-
function(data, X) {
  # Sort the data in ascending order
  sorted_data <- sort(data)
 

  # Find the number of values below X
  values_below_X <- sum(sorted_data < X)

 
  # Find the total number of values
  total_values <- length(sorted_data)
 

  # Calculate the percentile using the formula
  percentile <- (values_below_X + 0.5) / total_values * 100
 

  # Return the result
  cat(percentile,"%")
}

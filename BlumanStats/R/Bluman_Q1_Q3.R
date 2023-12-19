Bluman_Q1_Q3 <-
function(data) {

  # Sort the data in ascending order
  sorted_data <- sort(data)

  # Calculate the median (Q2)
  median_value <- median(sorted_data)

  # Split the data into lower and upper halves
  lower_half <- sorted_data[sorted_data < median_value]
  upper_half <- sorted_data[sorted_data > median_value]

  # Calculate Q1 and Q3 as the medians of the lower and upper halves
  Q1 <- median(lower_half)
  Q3 <- median(upper_half) 

  # Return the results
  return(list(Q1 = Q1, Q3 = Q3))
}

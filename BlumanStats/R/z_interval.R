z_interval <-
function(n, xbar, sigma, alpha) 
{
  #Calculate Confidence Level
  confid_level <- (1-alpha)*100
  
  # Calculate the margin of error
  margin_error_z <- qnorm(1 - alpha / 2) * (sigma / sqrt(n))
  
  # Calculate the lower and upper bounds of the confidence interval
  lower_bound_z <- xbar - margin_error_z
  upper_bound_z <- xbar + margin_error_z
  
  # Return the confidence interval as a named vector
  result <- c(confid_level,lower_bound_z, upper_bound_z, margin_error_z, xbar)
  names(result) <- c("% Confidence Level", "Lower Bound", "Upper Bound", "Margin of Error", "Sample Mean")
  return(result)
}

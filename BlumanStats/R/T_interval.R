T_interval <-
function(n, xbar, stdev, alpha) 
{
  #Calculate Conficence Level
  confid_level <- (1-alpha)*100
  
  #Calculate the t_value
  t_value <- qt(1-alpha/2,df=n-1)
  
  # Calculate the margin of error
  margin_error_t <- t_value * (stdev / sqrt(n))
  
  # Calculate the lower and upper bounds of the confidence interval
  lower_bound_t <- xbar - margin_error_t
  upper_bound_t <- xbar + margin_error_t
  
  # Return the confidence interval as a named vector
  result <- c(confid_level,lower_bound_t, upper_bound_t, margin_error_t, xbar)
  names(result) <- c("% Confidence Level", "Lower Bound", "Upper Bound", "Margin of Error", "Sample Mean")
  return(result)
}

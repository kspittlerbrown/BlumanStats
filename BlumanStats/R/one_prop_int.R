one_prop_int <-
function(num_successes, n, alpha) 
{
  #Calculate Conficence Level
  confid_level <- (1-alpha)*100
  
  #Calculate p-hat
  p_hat <- num_successes/n
  
  #Calculate the Z Critical Value
  z_critical_value <- qnorm(1 - alpha / 2) 
  
  #Calculate the standard error
  standard_error_prop <- sqrt((p_hat*(1-p_hat)/n))
  
  # Calculate the margin of error
  margin_error_prop <- standard_error_prop * z_critical_value
  
  # Calculate the lower and upper bounds of the confidence interval
  lower_bound_prop <- p_hat - margin_error_prop
  upper_bound_prop <- p_hat + margin_error_prop
  
  # Return the confidence interval as a named vector
  result <- c(confid_level,lower_bound_prop, upper_bound_prop, margin_error_prop, p_hat)
  names(result) <- c("% Confidence Level", "Lower Bound", "Upper Bound", "Margin of Error", "Sample Proportion")
  return(result)
}

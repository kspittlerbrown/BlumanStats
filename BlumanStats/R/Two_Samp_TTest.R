Two_Samp_TTest <-
function(xbar1,sd1,n1,xbar2,sd2,n2) 
{
  
  # Calculate sample variances
  var1 <- sd1^2
  var2 <- sd2^2
  
  # Calculate the standard error
  SE <-sqrt(var1/n1 + var2/n2)
  
  # Calculate t statistic
  t_stat <- (xbar1 - xbar2) / SE
  
  
  # Calculate degrees of freedom
  df <- min(n1,n2)-1
  
  # Calculate p-value
  p_value1 <- pt(abs(t_stat),df, lower.tail=FALSE)
  p_value2 <- 2 * pt(abs(t_stat), df, lower.tail=FALSE)
  
  # Create and return a result object  
  result <- c(t_stat, p_value1 ,p_value2 ,SE)
  names(result) <- c("Test Statistic","   One Tailed P-Value", "   Two Tailed P-Value", "   Standard Error")
  
  return(result)
}

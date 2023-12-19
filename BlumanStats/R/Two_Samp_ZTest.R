Two_Samp_ZTest <-
function(xbar1,sigma1,n1,xbar2,sigma2,n2) 
{
  
  # Calculate sample variances
  var1 <- sigma1^2
  var2 <- sigma2^2
  
  # Calculate the standard error
  SE <-sqrt(var1/n1 + var2/n2)
  
  # Calculate t statistic
  z_stat <- (xbar1 - xbar2) / SE
  
  # Calculate p-value
  p_value_2Tail_Test <- 2 * (1 - pnorm(abs(z_stat)))
  p_value_1Tail_test <- (1 - pnorm(abs(z_stat)))
  
  # Create and return a result object  
  result <- c(z_stat, p_value_1Tail_test,p_value_2Tail_Test,SE)
  names(result) <- c("Test Statistic","   One Tailed P-Value", "   Two Tailed P-Value", "   Standard Error")
  
  return(result)
}

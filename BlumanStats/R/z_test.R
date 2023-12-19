z_test <-
function(n,xbar,sigma, mu) 
{
  
  # Calculate the test statistic
  z <- (xbar - mu) / (sigma / sqrt(n))
  
  # Calculate the p-value
  p_value_2Tail_Test <- 2 * (1 - pnorm(abs(z)))
  p_value_1Tail_test <- (1 - pnorm(abs(z)))
  
  
  # Create a named vector with the results
  result <- c(z, p_value_1Tail_test, p_value_2Tail_Test)
  names(result)<-c("Test Statistic", "P-Value for One Tailed Test", "       P-Value Two Tailed Test")
  return(result)
}

T_test <-
function(n,xbar, stdev, mu) {
  
  # Calculate the test statistic
  t <- (xbar - mu) / (stdev / sqrt(n))
  
  # Calculate the p-value
  p_value_2Tail_Test <- 2 * pt(abs(t), df = n - 1, lower.tail=FALSE)
  p_value_1Tail_test <- pt(abs(t), df = n - 1, lower.tail=FALSE)
  
  # Create a named vector with the results
  result <- c(t, p_value_1Tail_test, p_value_2Tail_Test)
  names(result) <- c("Test Statistic", "P-Value for One Tailed Test", "     P-Value for Two Tailed Test")
  
  return(result)
}

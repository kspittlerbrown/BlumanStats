one_pop_prop_test <-
function(x,n,po){
  phat <- x/n
  numerator <- phat-po
  q <- 1-po
  denom <- sqrt((po*q)/n)

  #calculate the Test Statistic
  z <- numerator/denom
 

  # Calculate the p-value
  p_value_2Tail_Test <- 2 * (1 - pnorm(abs(z)))
  p_value_1Tail_test <- (1 - pnorm(abs(z)))
  cat("Sample Proportion:  ",phat,"\n")
  cat("Test Statistic:   z = ",z,"\n")
  cat("p-value for One Tail Test:  ",p_value_1Tail_test,"\n")
  cat("p-value for Two Tail Test:  ",p_value_2Tail_Test,"\n")
}

sign_rank_test <-
function(var1, var2, alpha){
  Diff <- var1 - var2
  print(Diff) 
  abs_diff <- abs(Diff)
  print(abs_diff)
  rank_diff <- rank(abs_diff)
  print(rank_diff)
  sign_rank <- sign(Diff)*rank_diff
  print(sign_rank)
  # Sum only the positive numbers
  sum_positive <- sum(sign_rank[sign_rank > 0])
  # Sum only the negative numbers
  sum_negative <- sum(sign_rank[sign_rank < 0])
  abs_sum_neg <- abs(sum_negative)
  cat("Sum of Positive Numbers:", sum_positive, "\n")
  cat("Sum of Negative Numbers:", sum_negative, "\n")
  #find min of the sums
  min_sum <- min(abs_sum_neg,sum_positive)
  
  
  #Find the number of pairs with difference not equal to zero
  n <- sum(before - after != 0)
  
  if (n <= 30) {
z <- min_sum
colname<-paste0("a",alpha)
colname2<-paste0("a",(alpha*2))
ntable <- c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)
  a0.1 <- c(1,2,4,6,8,11,14,17,21,26,30,36,41,47,54,60,68,75,83,92,101,110,120,130,141,152)
  a0.05 <- c(NA,1,2,4,6,8,11,14,17,21,25,30,35,40,46,52,59,66,73,81,90,98,107,117,127,137)
  a0.02 <- c(NA,NA,0,2,3,5,7,10,13,16,20,24,28,33,38,43,49,56,62,69,77,85,93,102,111,120)
  a0.01 <- c(NA,NA,NA,0,2,3,5,7,10,13,16,19,23,28,32,37,43,49,55,61,68,76,84,92,100,109)
tablek<-data.frame(ntable,a0.01,a0.02,a0.05,a0.1)
critical_value2=tablek[(tablek$ntable==n), colname]
critical_value=tablek[(tablek$ntable==n), colname2]
  cat("Test Statistic:",z,"\n")
cat("Sample Size:",n,"\n")
  cat("Critical Value (", "two-sided", "):", critical_value2, "\n")
  cat("Critical Value (", "one-sided", "):", critical_value, "\n")
cat("Reject H0 if the test statistic is less than the critical value.","\n")
  
  } else {
    # Calculate z based on your logic
    numerator <- n * (n + 1) / 4
    denom <- sqrt((n * (n + 1) * (2 * n + 1)) / 24)
    z <- (min_sum - numerator) / denom
    cat("Test Statistic:",z,"\n")
    # Calculate p-value
    p_value_2Tail_Test <- 2 * (1 - pnorm(abs(z)))
    p_value_1Tail_test <- (1 - pnorm(abs(z)))
    cat("Alpha:",alpha,"\n")
    cat("One Tailed P-Value",p_value_1Tail_test,"\n")
    cat("Two Tailed P-Value",p_value_2Tail_Test,"\n")
    cat("Reject H0 if the p-value is less than alpha.","\n")

  }
}

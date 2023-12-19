Two_Samp_Prop_Interval <-
function(x1, n1, x2, n2, alpha = 0.05) {
  # Calculate sample proportions
  p1 <- x1 / n1
  p2 <- x2 / n2
  q1 <- 1-p1
  q2 <- 1-p2
  
  # Calculate standard error & Point Estimate
  se <- sqrt((p1*q1/n1)+(p2*q2/n2))
  point_estimate <- p1-p2
  
  # Calculate critical value from the standard normal distribution
  z <- qnorm(1 - alpha/2)
  
  # Calculate margin of error
  margin_error <- z * se
  Conf_Level <- (1-alpha)*100
  # Calculate confidence interval
  lower_bound <- (p1 - p2) - margin_error
  upper_bound <- (p1 - p2) + margin_error
  
  cat("Confidence Level:", Conf_Level,"%   p-hat1=",p1, "  p-hat2=",p2,"\n")
  cat("Point Estimate:",point_estimate, "\n")
  cat("Margin of Error:",margin_error, "\n")
  cat("Critical Value:", z, "\n","\n")
  cat("Lower Bound:", lower_bound, "     Upper Bound:",upper_bound, "\n")
  
}

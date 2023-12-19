critical_z <-
function(alpha) {
  
  # Validate alpha input
  if (alpha <= 0 | alpha >= 1) {
    stop("Alpha must be between 0 and 1 exclusive.")
  }
  
  # Calculate Z critical value
  z_critical <- round(qnorm(1 - alpha),4)
  z_crit_left <- round(-1*qnorm(1 - alpha),4)
  z_critical2 <-round(qnorm(1 - alpha/2),4)
  neg_z_crit2 <- round(-1*z_critical2,4)
  
  result <-c(z_critical,z_crit_left,z_critical2,paste(neg_z_crit2, z_critical2, sep = " and "))
  names(result) <- c("Right Tail Test","Left Tail Test","   Confid Interval / Sample Size", "Two Tail Test")
return(result)
  }

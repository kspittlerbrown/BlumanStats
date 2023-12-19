critical_t <-
function(alpha, n) {
  
  # Validate alpha input
  if (alpha <= 0 | alpha >= 1) {
    stop("Alpha must be between 0 and 1 exclusive.")
  }
  
  # Validate sample size (n)
  if (n <= 0) {
    stop("Sample size (n) must be greater than 0.")
  }
  
  # Calculate t critical values
  t_critical <- round(qt(1 - alpha, df = n - 1), 4)
  t_crit_left <- round(-1 * qt(1 - alpha, df = n - 1), 4)
  t_critical2 <- round(qt(1 - alpha/2, df = n - 1), 4)
  neg_t_crit2 <- round(-1*t_critical2,4)
  
  result <- c(t_critical, t_crit_left, t_critical2,paste(neg_t_crit2, t_critical2, sep = " and "))
  names(result) <- c("Right Tail Test", "Left Tail Test", "Confid Interval","Two Tail Test")
  
  return(result)
}

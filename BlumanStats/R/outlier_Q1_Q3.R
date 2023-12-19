outlier_Q1_Q3 <-
function(Q1,Q3) {
  iqr <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * iqr
  upper_bound <- Q3 + 1.5 * iqr
  outlier_range <- c(lower_bound, upper_bound, iqr)
  names(outlier_range) <- c("Lower Bound","     Upper Bound", "IQR")
  return(outlier_range)
}

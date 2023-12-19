outlier_range <-
function(data) {
  q <- quantile(data, probs = c(0.25, 0.75))
  iqr <- q[2] - q[1]
  lower_bound <- q[1] - 1.5 * iqr
  upper_bound <- q[2] + 1.5 * iqr
  outlier_range <- c(lower_bound, upper_bound, iqr)
  names(outlier_range) <- c("Lower Bound","     Upper Bound", "IQR")
  return(outlier_range)
}

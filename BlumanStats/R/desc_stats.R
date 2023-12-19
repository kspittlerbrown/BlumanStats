desc_stats <-
function(data)
{
  std_error <- sd(data)/sqrt(length(data))
  result <- c(mean(data),sd(data), length(data), min(data), max(data), median(data), std_error)
  names(result) <- c("Mean", "Sample Standard Deviation", "n", "Minimum", "Maximum", "Median", "Standard Error")
  return(result)
}

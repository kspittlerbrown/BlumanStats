#Calculate the mode
mode_fcn <- function(data) {
  unique_values <- unique(data)
  counts <- table(match(data, unique_values))
  max_count <- max(counts)
  
  if (max_count == 1) {
    cat("No mode\n")
  } else {
    modes1 <- unique_values[counts == max_count]
    mode_counts <- data.frame(Value = modes1, Frequency = max_count)
    return(mode_counts)
  }
}

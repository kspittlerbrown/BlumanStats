rank_sum <-
function(var1,var2,alpha){
  
  # Create data frames with titles
  df_var1 <- data.frame(value = var1, title = "Group 1")
  df_var2 <- data.frame(value = var2, title = "Group 2")
  
  
  # Stack the data frames using rbind
  stacked_data <- rbind(df_var1, df_var2)
  
 # Display the result
 #print(stacked_data)
  
  #Sort the stacked data by the values.
  sorted_data <- stacked_data[order(stacked_data$value), ]
  
  
  # Add a new column for ranks
  sorted_data$rank <- rank(sorted_data$value)
  
  #Find Sample Sizes and Print the sorted ranked data.
  n1 <- length(var1)
  n2 <- length(var2)
  print(sorted_data)
  cat("\n","n1:",n1,"     n2:", n2,"\n","\n")
  
  # Calculate the sum of ranks for each group
  sum_ranks <- tapply(sorted_data$rank, sorted_data$title, sum)
  
  # Identify the group with the smaller sample size
  smaller_sample_group <- names(sort(table(stacked_data$title))[1])
  
  # Store the sum of ranks for the smaller sample size group
  sum_ranks_smaller_sample <- sum_ranks[smaller_sample_group]
  
  #cat("Sum of Ranks for Each Group:\n" , sum_ranks, " \n")
  #cat("Smaller Sample Size Group:", smaller_sample_group, "\n")
  #cat("Sum of Ranks for Smaller Sample Size Group:", sum_ranks_smaller_sample, "\n")
  
 #Logic to use in the mean and standard deviation formulas for the test statistic.
  min_size <- min(length(var1),length(var2))
  max_size <- max(length(var1),length(var2))
  mean_ranks <- (min_size*(min_size+max_size+1))/2
  stdev_ranks <-sqrt(((min_size*max_size)*(min_size+max_size+1))/12)
  
  #Calculate the test statistic
  z <- (sum_ranks_smaller_sample-mean_ranks)/stdev_ranks
  
  #Find the Critical Values
  critical_value_left <- qnorm(alpha, lower.tail = TRUE)
  critical_value_right <- qnorm(alpha, lower.tail = FALSE)
  critical_value_two_tailed <- qnorm(alpha/2, lower.tail = FALSE)
  
  cat("Critical Values:","\n")
  cat("Left Tail Test:",critical_value_left,"     Right Tail Test:",critical_value_right,"\n")

  cat("Two Tail Test:  ",critical_value_two_tailed," and ",-1*critical_value_two_tailed,"\n","\n")
  
  cat("Test Statistic:",z,"\n","\n")
  
 
  # Calculate p-value
  p_value_2Tail_Test <- 2 * (1 - pnorm(abs(z)))
  p_value_1Tail_test <- (1 - pnorm(abs(z)))
  
  
  # Create and return a result of p-values 
  result <- c(p_value_1Tail_test,p_value_2Tail_Test)
  names(result) <- c("One Tailed P-Value", "      Two Tailed P-Value")
  
  return(result)
  }

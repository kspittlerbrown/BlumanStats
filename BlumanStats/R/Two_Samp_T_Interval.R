Two_Samp_T_Interval  <- function(xbar1,sd1,n1,xbar2,sd2,n2,alpha){

  Conf_Level <- round((1-alpha)*100,2)
  point_est <- xbar1 - xbar2
  cat("Confidence Level: ",Conf_Level,"     Point Estimate:  ",point_est,"\n","\n")

  #Hand Calculations with Degrees of Freedom
  df_hand <- min(n1 - 1, n2 - 1)
  critical_value_hand <- round(qt(1-alpha/2, df_hand),5)
  Margin_Error_hand <- round(critical_value_hand*sqrt((sd1^2/n1)+(sd2^2/n2)),5)
  lower_bound_hand <- round(point_est - Margin_Error_hand,5)
  upper_bound_hand <- round(point_est+Margin_Error_hand,5)
  cat("*** Methods using degrees of freedom as the Minimum sample size - 1.   df: ",df_hand,"\n","\n")
  cat("Critical Value (min df):  ",critical_value_hand,"     Margin of Error (min df):",Margin_Error_hand,"\n")
  cat("Lower Bound (min df): ",lower_bound_hand,"\n")
  cat("Upper Bound (min df): ",upper_bound_hand,"\n","\n")

  #Technology Based Degrees of Freedom
  df <- round(((sd1^2/n1)+(sd2^2/n2))^2 / ((sd1^4/(n1^2*(n1-1))+(sd2^4/(n2^2*(n2-1))))),4)
  cat("*** Methods using Technology based degrees of freedom.  df: ",df,"\n","\n")
  critical_value <- round(qt(1-alpha/2, df),5)
  Margin_Error <- round(critical_value*sqrt((sd1^2/n1)+(sd2^2/n2)),5)
  lower_bound <- round(point_est - Margin_Error,5)
  upper_bound <- round(point_est+Margin_Error,5)
  result <- c(critical_value, Margin_Error,lower_bound,upper_bound)
  names(result) <- c("T Critical Value", "Margin of Error", "Lower Bound", "Upper Bound")
  return(result) 
}

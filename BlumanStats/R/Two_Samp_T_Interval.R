Two_Samp_T_Interval <-
function(xbar1,sd1,n1,xbar2,sd2,n2,alpha){
  #df <- min(n1 - 1, n2 - 1) Only Used for Textbook Answers
  df <- round(((sd1^2/n1)+(sd2^2/n2))^2 / ((sd1^4/(n1^2*(n1-1))+(sd2^4/(n2^2*(n2-1))))),4)
  critical_value <- round(qt(1-alpha/2, df),5)
  Conf_Level <- round((1-alpha)*100,2)
  point_est <- xbar1 - xbar2
   Margin_Error <- round(critical_value*sqrt((sd1^2/n1)+(sd2^2/n2)),5)
  lower_bound <- round(point_est - Margin_Error,5)
  upper_bound <- round(point_est+Margin_Error,5)
    result <- c(Conf_Level, critical_value, Margin_Error,point_est,df,lower_bound,upper_bound)
  names(result) <- c("Confidence Level %", "T Critical Value", "Margin or Error","Point Estimate","Degrees of Freedom", "Lower Bound", "Upper Bound")
  return(result)  
}

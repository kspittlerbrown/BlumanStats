Two_Samp_Z_Interval <-
function(xbar1,sigma1, n1,xbar2,sigmas2,n2,alpha){
  critical_value <- round(qnorm(1 - alpha/2),4)
  Conf_Level <- round((1-alpha)*100,2)
  point_est <- xbar1 - xbar2
  Margin_Error <- round(critical_value*sqrt((sigma1^2/n1)+(sigma2^2/n2)),4)
  lower_bound <- round(point_est - Margin_Error,4)
  upper_bound <- round(point_est+Margin_Error,4)
  
    result <- c(Conf_Level, critical_value, Margin_Error,point_est,lower_bound,upper_bound)
  names(result) <- c("Confidence Level %", "Critical Value", "Margin or Error","Point Estimate","Lower Bound", "Upper Bound")
    return(result)  
}

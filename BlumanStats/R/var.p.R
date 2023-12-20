#Caculate Population Variance
var.p <- function(data)
{
  mean <- mean(data)
  pop_Var <- (var(data)*(length(data) -1))/length(data)
  return(pop_Var)
}

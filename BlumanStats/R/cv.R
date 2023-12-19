cv <-
function(mean, std_dev){
  coef_var <- (std_dev/mean)*100
  cat("Coefficient of Variation: ",coef_var,"%","\n")
}

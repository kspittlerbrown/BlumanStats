var.p <-
function(data)
{
  pop_Var <- var(data)*length(data -1)/length(data)
  return(pop_Var)
}

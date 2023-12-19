se <-
function(data)
{
  std_error <- sd(data)/sqrt(length(data))
  return(std_error)
}

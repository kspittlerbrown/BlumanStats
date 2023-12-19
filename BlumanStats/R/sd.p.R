sd.p <-
function(data)
{
  population_std_dev <- sqrt(sum((data - mean(data))^2) / length(data))
  return(population_std_dev)
}

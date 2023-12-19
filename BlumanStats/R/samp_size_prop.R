samp_size_prop <-
function(alpha,p,E) {
  # Validate alpha input
  if (alpha <= 0 | alpha >= 1) {
    stop("Alpha must be between 0 and 1 exclusive.")
  }
  
   z_crit <-qnorm(1 - alpha/2)
  sample <- ceiling(((z_crit)^2 * p * (1-p)) / E^2)
  return(sample)
}

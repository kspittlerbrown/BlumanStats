samp_size_mean <-
function(alpha,sigma,E) {
  # Validate alpha input
  if (alpha <= 0 | alpha >= 1) {
    stop("Alpha must be between 0 and 1 exclusive.")
  }

  z_crit <-qnorm(1 - alpha/2)
  sample <- ceiling(((z_crit*sigma)/E)^2)
  return(sample)
}

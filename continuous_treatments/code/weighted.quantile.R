# Function to estimate quantiles with sampling weights
weighted.quantile <- function(x, q = .5, w = rep(1,length(x)), na.rm = T) {
  if (na.rm) {
    valid <- !is.na(x) & !is.na(w)
    w <- w[valid]
    x <- x[valid]
  }
  # Sort the weights by x
  w <- w[order(x)]
  # Sort x by x
  x <- x[order(x)]
  # Calculate the cumulative distribution function
  # at each observed data point
  cdf <- cumsum(w) / sum(w)
  # Find the first index where the CDF exceeds the quantile cutoff
  first_index <- min(which(cdf > q))
  # Return the x-value at that index
  return(x[first_index])
}
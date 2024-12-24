fastSumOfBernoulli <- function(p) {
  n <- length(p)
  if (n == 1) {
    return(c(1 - p, p))
  }
  mid <- floor(n / 2)
  left_p <- fastSumOfBernoulli(p[1:mid]) 
  right_p <- fastSumOfBernoulli(p[(mid + 1):n]) 
  result <- convolve(left_p, rev(right_p), type = "open")
  result <- result[1:(n + 1)]
  result <- pmax(0, pmin(1, result))
  result <- result / sum(result)
  return(result)
}
c(0.1,0.2,0.3,0.4)

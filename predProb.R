predProb <- function(x) {
  p <- length(x)
  lg <- numeric(5)
  for (k in 1:5) {
    betak0hat <- 2^(-k)
    betakjhat <- sapply(1:p, function(j) 2^abs(k - j))
    lg[k] <- betak0hat + sum(betakjhat * x)
  }
  lg <- lg - max(lg)
  pr <- exp(lg) / sum(exp(lg))
  return(pr)
}

nearestNeighborInterpolation <- function(X, y, Z) {
  p <- ncol(X)
  n <- nrow(X)
  result <- numeric(nrow(Z))
  
  for (j in 1:nrow(Z)) {
    z <- Z[j, ]
    D <- sqrt(colSums((t(X) - z)^2))
    nearest <- order(D)[1:(p + 1)]
    A <- X[nearest, ]
    b <- y[nearest]
    ls <- cbind(rep(1, p + 1), A)
    solution <- solve(t(ls) %*% ls, t(ls) %*% b)
    beta_0 <- solution[1]
    beta_hat <- solution[-1]
    result[j] <- beta_0 + sum(z * beta_hat)
  }
  return(result)
}
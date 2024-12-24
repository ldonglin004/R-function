piecewiseLinearInterpolation <- function(X, Y, Z) {
  A <- cbind(X^2, X, rep(1, length(X)))
  coef <- solve(t(A) %*% A, t(A) %*% Y)
  a <- coef[1]
  b <- coef[2]
  c <- coef[3]
  f.true <- a * Z^2 + b * Z + c
  f.hat <- numeric(length(Z))
  order_index <- order(X)
  X <- X[order_index]
  Y <- Y[order_index]
  for (j in 1:length(Z)) {
    z <- Z[j]
    if (z <= X[1]) {
      x1 <- X[1]
      x2 <- X[2]
      y1 <- Y[1]
      y2 <- Y[2]
    } else if (z >= X[length(X)]) {
      x1 <- X[length(X) - 1]
      x2 <- X[length(X)]
      y1 <- Y[length(Y) - 1]
      y2 <- Y[length(Y)]
    } else {
      for (i in 1:(length(X) - 1)) {
        if (X[i] <= z && z <= X[i+1]) {
          x1 <- X[i]
          x2 <- X[i + 1]
          y1 <- Y[i]
          y2 <- Y[i + 1]
          break
        }
      }
    }
    f.hat[j] <- y1 + (y2 - y1) / (x2 - x1) * (z - x1)
  }
  return(data.frame(f.hat = f.hat, f.true = f.true))
}




fastRidgeRegression <- function(x, y, lambda) {
  p <- ncol(x)
  n <- nrow(x)
  if (n >= p) {
    U <- chol(crossprod(x)+lambda * diag(p))
    z <- forwardsolve(U, crossprod(x, y), upper.tri = TRUE, transpose = TRUE)
    beta_hat <- backsolve(U, z)
  } else {
    U_1 <- chol(x %*% t(x) + lambda * diag(n))
    z_1 <- forwardsolve(U_1, y, upper.tri = TRUE, transpose = TRUE)
    beta_hat <- crossprod(x, backsolve(U_1, z_1))
  }
  
  beta_hat_rounded <- round(beta_hat)
  nonzero_idx <- which(beta_hat_rounded != 0)
  result <- data.frame(
    index = nonzero_idx,
    beta = beta_hat_rounded[nonzero_idx]
  )
  return(result)
}

kernelRidgeRegression <- function(df, lambda, rho, bw) {
  x_train <- df$x_train
  y_train <- df$y_train
  x_test  <- df$x_test
  y_test  <- df$y_test
  
  sparsekernel <- function(x, y) {
    n <- length(x)
    indices <- which(abs(outer(x, y, FUN = "-")) <= bw, arr.ind = TRUE)
    diff <- x[indices[, 1]] - y[indices[, 2]]
    kernel <- exp(-rho * (diff ^ 2))
    sparse <- sparseMatrix(i = indices[, 1], 
                           j = indices[, 2], 
                           x = kernel, 
                           dims = c(n, length(y)))

    return(sparse)
  }
  
  beta_hat <- solve(sparsekernel(x_train, x_train) + Diagonal(length(x_train), x = lambda), y_train)
  y_pre <- sparsekernel(x_test, x_train) %*% beta_hat
  pmse <- mean((y_test - y_pre)^2)
  return(pmse)
}

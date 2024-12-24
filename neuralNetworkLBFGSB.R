neuralNetworkLBFGSB <- function(p, df) {
  GeLu <- function(x) {
    x * pnorm(x)
  }
  
  GeLu_derivative <- function(x) {
    pnorm(x) + dnorm(x) * x
  }
  
  compute_Y_hat <- function(alpha, X, p) {
    n <- length(X)
    Y_hat <- rep(alpha[1], n)
    for (j in 1:p) {
      c <- alpha[p + j + 1] + alpha[2 * p + j + 1] * X
      Y_hat <- Y_hat + alpha[j + 1] * GeLu(c)
    }
    return(Y_hat)
  }
  
  network_model <- function(alpha, X, Y, p) {
    Y_hat <- compute_Y_hat(alpha, X, p)
    mse <- mean((Y - Y_hat)^2)
    return(mse)
  }
  
  gradient <- function(alpha, X, Y, p) {
    n <- length(Y)
    grad <- numeric(length(alpha))
    Y_hat <- compute_Y_hat(alpha, X, p)
    residual <- Y - Y_hat
    
    grad[1] <- -2 * mean(residual)
    for (j in 1:p) {
      c <- alpha[p + j + 1] + alpha[2 * p + j + 1] * X
      gelu_c <- GeLu(c)
      gelu_deriv_c <- GeLu_derivative(c)
      
      grad[j + 1] <- -2 * mean(residual * gelu_c)
      grad[p + j + 1] <- -2 * mean(residual * alpha[j + 1] * gelu_deriv_c)
      grad[2 * p + j + 1] <- -2 * mean(residual * alpha[j + 1] * gelu_deriv_c * X)
    }
    
    return(grad)
  }
  
  initial_alpha <- runif(3 * p + 1, min = -1, max = 1)
  lower_bounds <- rep(-10, 3 * p + 1)
  upper_bounds <- rep(10, 3 * p + 1)
  result <- optim(
    par = initial_alpha,
    fn = network_model,
    gr = gradient,
    X = df$X,
    Y = df$Y,
    p = p,
    method = "L-BFGS-B",
    lower = lower_bounds,
    upper = upper_bounds,
    control = list(maxit = 10000)
  )
  return(result)
}

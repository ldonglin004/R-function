nonlinearLogisticRegression <- function(df) {
  logistic_score <- function(alpha, X, Y, Z) {
    eta <- alpha * X - alpha^2 * Z^2
    p <- exp(eta) / (1 + exp(eta))
    score <- sum((Y - p) * (X - 2 * alpha * Z^2)) 
    return(score)
  }
  
    secant <- function(f, x0, x1, tol=1e-10, max_iter=1000) {
    convergence <- 1
    f0 <- f(x0); f1 <- f(x1)
    
    if(abs(f0 - f1) < tol) {
      warning("Initial values too close, derivative may be near-zero.")
      return(list(root = NA, f_root = NA, iter = 0, convergence = 1))
    }
    
    for(iter in 1:max_iter) {
      x_diff <- -f1 / (f1 - f0) * (x1 - x0)
      x2 <- x1 + x_diff
      if(abs(x_diff) < tol) {
        convergence <- 0
        break
      }
      x0 <- x1
      f0 <- f1
      x1 <- x2
      f1 <- f(x2)
      if(abs(f1 - f0) < tol) {
        warning("Convergence issue, step size too small.")
        break
      }
    }
    
    return(list(root = x2, f_root = f(x2), iter = iter, convergence = convergence))
  }
  
  X <- df$X
  Y <- df$Y
  Z <- df$Z
  

  score_function <- function(alpha) logistic_score(alpha, X, Y, Z)
  

  initial_alpha_1 <- 1
  initial_alpha_2 <- -1
  

  result <- secant(score_function, initial_alpha_1, initial_alpha_2, tol = 1e-10, max_iter = 1000)

  if(result$convergence == 0) {
    return(round(result$root, 5))
  } else {
    warning("Secant method did not converge.")
    return(NA)
  }
}

# df = read.table('test.1.tsv', header=TRUE)
# print(head(df),row.names = FALSE)
# nonlinearLogisticRegression(df)
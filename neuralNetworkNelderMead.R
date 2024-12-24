neuralNetworkNelderMead <- function(p, df) {
  
  GeLu <- function(x) {
    x * pnorm(x)
  }
  
  network_model <- function(alpha, X, Y, p) {
    n <- length(Y)
    Y_hat <- rep(alpha[1], n)
    for (j in 1:p) {
      c <- alpha[p + j + 1] + alpha[2 * p + j + 1] * X
      Y_hat <- Y_hat + alpha[j + 1] * GeLu(c)
    }
    mse <- mean((Y - Y_hat)^2)
    eval_count <<- eval_count + 1  
    return(mse)
  }
  
  Nelder.Mead <- function(f, x0, tol = 1e-5, max_iter = 10000, ...) {
    d <- length(x0)  
    X <- matrix(x0, nrow = d, ncol = d + 1)  
    X[,-(d+1)] <- X[,-(d+1)] + diag(d)  
    Y <- apply(X, 2, f, ...)  
    
    idx_max <- NULL; idx_min <- NULL; idx_2ndmax <- NULL
    mid_point <- NULL; tru_line <- NULL
    
    update.extremes <- function() {
      if (Y[1] > Y[2]) {
        idx_max <<- 1; idx_min <<- 2; idx_2ndmax <<- 2
      } else {
        idx_max <<- 2; idx_2ndmax <<- 1; idx_min <<- 1
      }
      if (d > 1) {
        for (i in 3:(d + 1)) {
          if (Y[i] <= Y[idx_min]) {
            idx_min <<- i
          } else if (Y[i] > Y[idx_max]) {
            idx_2ndmax <<- idx_max; idx_max <<- i
          } else if (Y[i] > Y[idx_2ndmax]) {
            idx_2ndmax <<- i
          }
        }
      }
    }
    
    update.mid.point <- function() {
      mid_point <<- apply(X[,-idx_max, drop = FALSE], 1, mean)
      tru_line <<- X[, idx_max] - mid_point
    }
    
    update.next.point <- function(step_scale) {
      next_point <- mid_point + tru_line * step_scale
      Y_next <- f(next_point, ...)
      if (Y_next < Y[idx_max]) {
        X[, idx_max] <<- next_point
        Y[idx_max] <<- Y_next
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
    
    contract.simplex <- function() {
      X[,-idx_min] <<- 0.5 * (X[,-idx_min] + X[, idx_min])
      Y[-idx_min] <<- apply(X[,-idx_min], 2, f, ...)
    }
    
    convergence <- 1
    for (iter in 1:max_iter) {
      update.extremes()
      if (abs(Y[idx_max] - Y[idx_min]) <= tol * (abs(Y[idx_max]) + abs(Y[idx_min]) + tol)) {
        convergence <- 0
        break
      }
      update.mid.point()
      
      update.next.point(-1.0)
      if (Y[idx_max] < Y[idx_min]) {
        update.next.point(-2.0)
      } else if (Y[idx_max] >= Y[idx_2ndmax]) {
        if (!update.next.point(0.5)) {
          contract.simplex()
        }
      }
    }
    
    return(list(xmin = X[, idx_min],
                fmin = Y[idx_min],
                convergence = convergence,
                iter = iter))
  }
  
  X <- df$X
  Y <- df$Y
  alpha_init <- rep(0, 3 * p + 1)  
  eval_count <- 0  

  result <- Nelder.Mead(f = function(alpha) network_model(alpha, X, Y, p),
                        x0 = alpha_init,
                        tol = 1e-5,
                        max_iter = 10000)
  
  return(c(result$fmin, result$iter, eval_count))
}

# Example usage
# df <- read.table('test.1.tsv', header=TRUE)
# neuralNetworkNelderMead(10, df)


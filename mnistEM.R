mnistEM <- function(X, nclust, max.iter = 75, tol = 1e-4) {
  X <- ifelse(X > 128, 1, 0)
  n <- nrow(X)
  p <- ncol(X)
  
  z <- sample(1:nclust, n, replace = TRUE)
  pi_k <- table(z) / n
  theta <- matrix(runif(nclust * p, 0.25, 0.75), nclust, p)
  theta <- pmax(pmin(theta, 1 - 1e-8), 1e-8)
  log_likelihood <- -Inf  
  
  for (iter in 1:max.iter) {
    # E-step
    log_gamma <- matrix(0, nrow = n, ncol = nclust)
    for (k in 1:nclust) {
      log_prob <- X %*% log(theta[k, ]) + (1 - X) %*% log(1 - theta[k, ])
      log_gamma[, k] <- log(pi_k[k]) + log_prob
    }
    max_log_gamma <- apply(log_gamma, 1, max) 
    log_sum_exp <- max_log_gamma + log(rowSums(exp(log_gamma - max_log_gamma)))
    gamma <- exp(log_gamma - log_sum_exp) 
    
    # Compute log-likelihood
    new_log_likelihood <- sum(log_sum_exp)  
    if (abs(new_log_likelihood - log_likelihood) < tol) break  
    log_likelihood <- new_log_likelihood
    
    # M-step
    pi_k <- colSums(gamma) / n 
    for (k in 1:nclust) {
      theta[k, ] <- colSums(gamma[, k] * X) / sum(gamma[, k])  
      theta[k, ] <- pmax(pmin(theta[k, ], 1 - 1e-8), 1e-8)
    }
  }
  est <- apply(gamma, 1, which.max)  
  return(list(est = est, iter = iter, log_likelihood = log_likelihood, theta = theta, pi = pi_k))
}

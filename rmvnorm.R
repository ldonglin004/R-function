rmvnorm <- function(n, p, rho) {
  # 1. Vectorized computation of the covariance matrix Σ(ρ)
  l <- (0:(p - 1)) / p  # Vector of li values
  dist_matrix <- abs(outer(l, l, "-"))^1.99  # Matrix of |li - lj|^1.99
  cos_values <- abs(cos(l))  # Vector of |cos(li)|
  Sigma <- exp(p * log(rho) * dist_matrix - outer(cos_values, cos_values, "+"))
  
  # 2. Generate multivariate normal samples using Cholesky decomposition
  chol_Sigma <- chol(Sigma)  # Cholesky decomposition of Σ(ρ)
  random_samples <- matrix(rnorm(n * p), nrow = n) %*% chol_Sigma
  
  # 3. Efficient calculation of the required statistics
  # a. Estimate E{maxj Xj}
  estimate_max_Xj <- mean(apply(random_samples, 1, max))
  
  # b. Estimate E{sqrt(sum(Xj^2) for j = 1 to p)}
  estimate_sqrt_sum_Xj_sq <- mean(sqrt(rowSums(random_samples^2)))
  
  # c. Estimate Pr(X1 * X2 > 0.5 * rho)
  prob_X1X2 <- mean(random_samples[, 1] * random_samples[, 2] > 0.5 * rho)
  
  # Return results as a numeric vector
  c(estimate_max_Xj, estimate_sqrt_sum_Xj_sq, prob_X1X2)
}

# Example to test:
# Uncomment to test the function with a sample input
# print(rmvnorm(100000, 80, 0.42), digits=1)
# print(rmvnorm(100000, 103, 0.63), digits=1)
